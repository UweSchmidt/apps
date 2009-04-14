module Main
where

import           Control.Arrow

-- import           Control.Concurrent

import           Control.Monad       	( filterM
					, when
					)
import           Control.Monad.Trans 	( liftIO )

import           Data.Either
import qualified Data.Function as F  	( on )
import           Data.IORef
import           Data.List	   	( delete
					, elemIndex
					, isPrefixOf
					, nub
					, sort
					, sortBy
					)
import qualified Data.Map as M
import           Data.Maybe

import           Graphics.UI.Gtk
import           Graphics.UI.Gtk.Glade

import           Photo2.ArchiveTypes	( nats )
import qualified Photo2.ArchiveTypes as AT
import           Photo2.Config		( keyKeywords )
import		 Photo2.ModelInterface
import		 Photo2.FilePath

import           System.IO
import           System.Directory
import           System.IO.Unsafe      	( unsafePerformIO )

import qualified Text.XML.HXT.Arrow   as T
import qualified Text.XML.HXT.RelaxNG as NG

-- ------------------------------------------------------------

type Getter s a         = s -> a
type Setter s a         = a -> s -> s
type Selector s a       = (Getter s a, Setter s a)

sub                     :: Selector b c -> Selector a b -> Selector a c
sub (g2, s2) (g1, s1)   = ( g2 . g1
                          , s1s2
                          )
                          where
                          s1s2 x s = s'
                              where
                              x1  = g1 s
                              x1' = s2 x x1
                              s'  = s1 x1' s

sel1	:: Selector (a, b) a
sel1	= (fst, \ f (_, s) -> (f, s))

sel2	:: Selector (a, b) b
sel2    = (snd, \ s (f, _) -> (f, s))

-- ------------------------------------------------------------

type Maybe12 a b	= (a, Maybe b)

map12			:: (a -> a1) -> (b -> b1) -> Maybe12 a b -> Maybe12 a1 b1
map12 f g		= f *** fmap g

map12M			:: Monad m => (a -> m a1) -> (b -> m b1) -> Maybe12 a b -> m (Maybe12 a1 b1)
map12M f g (x, y)	= do
			  x1 <- f x
			  y1 <- maybe ( return Nothing )
				      (\ v -> g v >>= return . Just )
                                      $ y
			  return (x1, y1)

-- ------------------------------------------------------------

type Attrs		= [(String, String)]

-- ------------------------------------------------------------

type ButtonPair		= Maybe12 ImageMenuItem ToolButton

xmlGetButtonPair	:: GladeXML -> Maybe12 String String -> IO ButtonPair
xmlGetButtonPair gx	= map12M (xmlGetWidget gx castToImageMenuItem)
				 (xmlGetWidget gx castToToolButton   )

onActivateBP		:: ButtonPair ->
			   String ->
			   IO () ->
			   IO ( Maybe12 (ConnectId ImageMenuItem)
				(ConnectId ToolButton)
			      )
onActivateBP bp n act	= do
			  map12M (`onActivateLeaf`      act')
				 (`onToolButtonClicked` act')
                                 $ bp
			  where
			  act'
			      = do
				trcMsg ("start action " ++ show n)
				act
				trcMsg ("end   action " ++ show n)

-- ------------------------------------------------------------

loadGladeModel :: FilePath -> IO GladeXML
loadGladeModel fn
    = do
      m <- xmlNew fn
      return (fromMaybe err m)
    where
    err = error $
	  unwords [ "can' find the glade file"
		  , show fn
		  , "in the current directory"
		  ]
-- ------------------------------------------------------------

main = do
  initGUI

  xwins      <- mapM loadGladeModel $
		map ("config/photoEdit/" ++) $
			[ "MainWindow.glade"
			, "QuitDialog.glade"
			, "AlbumIdDialog.glade"
			, "AttributeDialog.glade"
			]
  initApp 	xwins

  -- timeoutAddFull (yield >> return True)
  --                    priorityDefaultIdle 100

  withGUI  	installGUIControls

  changeLogger	installStatusbarLogger

  withWindow  	widgetShowAll
  openArchive
  mainGUI

-- ------------------------------------------------------------

type AppState	= IORef App

data App = App { gui  :: GUI
	       , dat  :: Model
	       , cntl :: Control
	       }

theId           :: Selector a a
theId		= (id, const)

theGUI		:: Selector App GUI
theGUI		= (gui, \ g a -> a {gui = g})

theModel	:: Selector App Model
theModel	= (dat, \ m a -> a {dat = m})

theCtrl		:: Selector App Control
theCtrl		= (cntl, \ c a -> a {cntl = c})

-- ------------------------------------------------------------

data GUI = GUI { window    :: Window
	       , statusbar :: Statusbar
	       , tabs      :: Notebook
	       , lightbox  :: Lightbox
	       , controls  :: Buttons
	       , quitdlg   :: Dialog
	       , albumdlg  :: EDialog 
	       , attrdlg   :: ADialog
	       }

type Lightbox	= (ScrolledWindow, Table)
type Buttons	= [ButtonPair]
type EDialog    = (Dialog, Entry)
type ADialog    = (Dialog, Table)

-- ------------------------------------------------------------

theWindow		= (window,    \ w g -> g {window    = w})
theStatusbar		= (statusbar, \ s g -> g {statusbar = s})
theTabs         	= (tabs,      \ t g -> g {tabs      = t})

theLightbox     	= (lightbox,  \ c g -> g {lightbox  = c})
theLightboxWindow       = sel1 `sub` theLightbox
theLightboxTable	= sel2 `sub` theLightbox

theButtons      = (controls,  \ b g -> g {controls  = b})
theQuitDlg	= (quitdlg,   \ d g -> g {quitdlg   = d})
theAlbumDlg     = (albumdlg,  \ d g -> g {albumdlg  = d})
theAttrDlg      = (attrdlg,   \ d g -> g {attrdlg   = d})

-- ------------------------------------------------------------

data Control 	= Cntl { logger	  :: Logger
		       , selected :: [[String]]		-- the selections in all tabs
		       , currTab  :: Int
		       }

type Logger	= String -> IO ()

-- ------------------------------------------------------------

theLogger	= (logger,   \ l c -> c {logger   = l})
theSelected	= (selected, \ s c -> c {selected = s})
theCurrTab	= (currTab,  \ t c -> c {currTab  = t})
theCurrSelected	= ( \ c   -> selected c !! currTab c
                  , \ s c -> c {selected = setAt (currTab c) s (selected c)}
		  )
theCbSelected	= (head, \ s ss -> s : tail ss) `sub` theSelected

-- ------------------------------------------------------------
--
-- the single global variable containing the whole
-- application

{-# NOINLINE appState #-}
appState		:: IORef App
appState		= unsafePerformIO $
		  	  newIORef $
		  	  App undefined undefined undefined

-- ------------------------------------------------------------

changeApp		:: (App     -> IO App    ) -> IO ()
changeGUI		:: (GUI     -> IO GUI    ) -> IO ()
changeModel             :: (Model   -> IO Model  ) -> IO ()
changeCtrl              :: (Control -> IO Control) -> IO ()

changeApp action	= do
			  state0 <- readIORef appState
			  state1 <- action state0
			  writeIORef appState state1

changeGUI 		= changeComp theGUI
changeModel		= changeComp theModel
changeCtrl		= changeComp theCtrl
changeLogger            = changeComp (theLogger       `sub` theCtrl)
changeSelected          = changeComp (theSelected     `sub` theCtrl)
changeCurrSelected      = changeComp (theCurrSelected `sub` theCtrl)

-- ------------------------------------------------------------

setApp			:: App     -> IO ()
setGUI			:: GUI     -> IO ()
setModel                :: Model   -> IO ()
setCtrl                 :: Control -> IO ()

setApp	a		= changeApp $ \ _ -> return a
setGUI			= setComp theGUI
setModel                = setComp theModel
setCtrl                 = setComp theCtrl

setLightbox		= setComp (theLightbox      `sub` theGUI)
setSelected             = setComp (theSelected      `sub` theCtrl)
setCurrSelected         = setComp (theCurrSelected  `sub` theCtrl)
setClipboardSelected    = setComp (theCbSelected    `sub` theCtrl)
setCurrTab              = setComp (theCurrTab       `sub` theCtrl)

-- ------------------------------------------------------------

withApp			:: (App     -> IO a) -> IO a
withGUI			:: (GUI     -> IO a) -> IO a
withModel               :: (Model   -> IO a) -> IO a
withCtrl                :: (Control -> IO a) -> IO a
withApp action		= do
			  state0 <- readIORef appState
			  action state0

withGUI			= withComp theGUI
withModel               = withComp theModel
withCtrl                = withComp theCtrl

withWindow		= withComp (theWindow    `sub` theGUI)
withStatusbar		= withComp (theStatusbar `sub` theGUI)
withTabs                = withComp (theTabs      `sub` theGUI)

withLightbox            = withComp (theLightbox       `sub` theGUI)
withLightboxTable       = withComp (theLightboxTable  `sub` theGUI)
withLightboxWindow      = withComp (theLightboxWindow `sub` theGUI)
withAlbumDialog         = withComp (theAlbumDlg       `sub` theGUI)
withAttrDialog          = withComp (theAttrDlg        `sub` theGUI)

withLogger              = withComp (theLogger         `sub` theCtrl)
withSelected            = withComp (theSelected       `sub` theCtrl)
withCurrSelected        = withComp (theCurrSelected   `sub` theCtrl)
withClipboardSelected   = withComp (theCbSelected     `sub` theCtrl)
withCurrTab             = withComp (theCurrTab        `sub` theCtrl)

-- ------------------------------------------------------------

getModel                = withModel             return

getLightbox		= withLightbox		return
getLightboxTable	= withLightboxTable	return
getSelected             = withSelected		return
getCurrSelected         = withCurrSelected	return
getClipboardSelected	= withClipboardSelected	return
getClipboard		= withClipboard		return
getAlbumDialog          = withAlbumDialog       return
getAttrDialog           = withAttrDialog        return

getLogger               = withLogger		return

getAllAlbumPaths	= withTabs           $ withLightboxTables widgetGetName
getCurrAlbumPath	= withLightboxTable  $ widgetGetName
getLastSelectedPath	= do
			  path <- getCurrAlbumPath
			  ss   <- getCurrSelected
			  if null ss
			     then return ""
			     else return $ path </> head ss
		          
-- ------------------------------------------------------------
--
-- the combinators for actions on subcomponents

modifyComp		:: Selector s a -> (a -> IO a) -> (s -> IO s)
modifyComp (get, set) action
    			= \ state0 ->
			  let c0 = get state0
			  in do
			     c1 <- action c0
			     return $ set c1 state0

onComp			:: Selector s a -> (a -> IO b) -> (s -> IO b)
onComp (get, _) action
			= \ state0 ->
			  let c0 = get state0
			  in do
			     action c0

-- ------------------------------------------------------------

changeComp              :: Selector App a -> (a -> IO a) -> IO ()
changeComp sel 		= changeApp . modifyComp sel

setComp                 :: Selector App a -> a -> IO ()
setComp sel value	= changeApp . modifyComp sel $ (const $ return value)

withComp                :: Selector App a1 -> (a1 -> IO a) -> IO a
withComp sel		= withApp . onComp sel

-- ------------------------------------------------------------

initApp	:: [GladeXML] -> IO ()
initApp ws
    = do
      gui <-  buildGUI ws
      mdl <-  buildModel
      setGUI   $ gui
      setModel $ mdl
      setCtrl  $ initControl

-- ------------------------------------------------------------

initControl	:: Control
initControl	= Cntl { logger   = hPutStrLn stderr
		       , selected = [[]]
		       , currTab  = 0
		       }

-- ------------------------------------------------------------

logMsg		:: String -> IO ()
logMsg msg	= withLogger ($ msg)

trcMsg		:: String -> IO ()
{-
trcMsg		= logMsg . ("Trace:   " ++)
-}
trcMsg		= hPutStrLn stderr . ("-- (t)   " ++)

warnMsg		:: String -> IO ()
warnMsg		= logMsg . ("Warning: " ++)


-- ------------------------------------------------------------

buildGUI	:: [GladeXML] -> IO GUI
buildGUI [gm, {- gl, -} qd, ad, at]
    = do
      window    <- xmlGetWidget gm castToWindow         "main"
      statusbar <- xmlGetWidget gm castToStatusbar      "statusbar"
      tabs      <- xmlGetWidget gm castToNotebook       "tabs"
      lightbox  <- xmlGetWidget gm castToScrolledWindow "clipboardwindow"
      lbxtable  <- xmlGetWidget gm castToTable          clipboard
      controls  <- buildButtons gm

      quitDlg   <- xmlGetWidget qd castToDialog         "quitDialog"
      albumDlg  <- xmlGetWidget ad castToDialog         "albumDialog"
      albumEnt  <- xmlGetWidget ad castToEntry          "albumId"
      attrDlg   <- xmlGetWidget at castToDialog         "attrDialog"
      attrTbl   <- xmlGetWidget at castToTable          "attrTable"

      return $ GUI window statusbar tabs (lightbox, lbxtable)
	           controls
		   quitDlg (albumDlg, albumEnt) (attrDlg, attrTbl)

buildButtons	:: GladeXML -> IO Buttons
buildButtons gm
    = do
      buttons <- mapM (xmlGetButtonPair gm) $
		 [ ("open", 		Just "tbopen"		)      	-- file menu
		 , ("save", 		Just "tbsave"		)
		 , ("close", 		Just "tbclose"		)
		 , ("quit",		Nothing			)

		 , ("movecb",		Just "tbmovecb"		)	-- edit menu
		 , ("copycb",		Just "tbcopycb"		)
		 , ("cbmove",		Just "tbcbmove"		)
		 , ("cbcopy",		Just "tbcbcopy"		)
		 , ("cbdelete",		Just "tbcbdelete"	)

		 , ("newalbum",		Nothing			)
		 , ("newpic",		Nothing			)

		 , ("import",		Nothing			)
		 , ("edit",             Just "tbedit"           )

		 , ("align",		Just "tbalign"		)
		 , ("sort",		Just "tbsort"		)
		 , ("update",           Nothing			)
		 , ("html",             Nothing			)
		 , ("invert",		Just "tbinvert"		)
		 , ("deselect",		Just "tbdeselect"	)

		 , ("test",	 	Just "tbtest"		)
		 , ("test2",	 	Nothing			)
		 ]
      return buttons

clipboard	= "/clipboard"

-- ------------------------------------------------------------

installGUIControls	:: GUI -> IO ()
installGUIControls g
    = do
      mw `onDelete` (const $ do
		             quitDialog
		             return True
		    )
      mw `onDestroy` mainQuit

      mw `on` configureEvent   $ liftIO resize
      mw `on` enterNotifyEvent $ liftIO resize		-- this is a hack, but what's the right event

      ts `onSwitchPage` switchTab

      installButtonControls
    where
    mw = window    g
    ts = tabs      g
    bx = lightbox  g
    cs = controls  g
    qd = quitdlg   g

    resize	= do
		  resizeLightboxTable
		  return False
    
    installButtonControls	:: IO ()
    installButtonControls
	= do
	  onActivateBP open 	"open" 			openSelectedAlbum
	  onActivateBP save     "save album"	        storeTab
	  onActivateBP close    "close album"		closeTab
	  onActivateBP quit 	"quit"			quitDialog

	  onActivateBP movecb  	"move to clipboard"     $ copySelectionToClipboard    True
	  onActivateBP copycb  	"copy to clipboard"     $ copySelectionToClipboard    False
	  onActivateBP cbmove  	"move from clipboard"   $ copySelectionFromClipboard  True
	  onActivateBP cbcopy  	"copy from clipboard"   $ copySelectionFromClipboard  False
	  onActivateBP cbdelete	"delete from clipboard" $ deleteSelectedFromClipboard

	  onActivateBP newalbum	"make pic to album"     $ newSelectedAlbum
	  onActivateBP newpic	"make album to pic"     $ newSelectedPic
	  onActivateBP import'	"import pictures"       $ importPictures
	  onActivateBP editAttr "edit attributes"       $ editAttributes

	  onActivateBP align    "align"         resizeLightboxTable
	  onActivateBP sort     "sort"          sortSelected
          onActivateBP update   "update"        updateAlbum
          onActivateBP html     "html"          htmlAlbum
	  onActivateBP invert   "invert"        invertSelected
	  onActivateBP deselect "deselect"      clearSelected
	  onActivateBP test     "test"          testOP1
	  onActivateBP test2    "test2"         testOP2
	  return ()
	where
	[ open, save, close, quit,
	  movecb, copycb, cbmove, cbcopy, cbdelete,
	  newalbum, newpic, import', editAttr,
	  align, sort, update, html, invert, deselect,
	  test, test2] = cs

    quitDialog
	= do
	  stateChanged <- return True
	  if stateChanged
	     then do
		  res <- dialogRun qd
		  widgetHide       qd
		  quitApp res
	     else do
		  quitApp ResponseNo

-- ------------------------------------------------------------

albumDialog	:: IO String
albumDialog
    = do
      (ad, en)  <- getAlbumDialog
      entrySetText              en ""
      res       <- dialogRun    ad
      widgetHide                ad
      str	<- entryGetText en
      return $ evalRes res str
    where
    evalRes ResponseOk s        = s
    evalRes _		_	= ""

-- ------------------------------------------------------------

attrDialog	:: Attrs -> IO Attrs
attrDialog as
    = do
      (ad, at)	<- getAttrDialog
      tableSetAttr              at
      res	<- dialogRun	ad
      widgetHide		ad
      el	<- tableGetAttr at
      return $ evalRes res el
    where
    evalRes ResponseOk es	= es
    evalRes _	       _	= []

    tableSetAttr tbl
	= do
	  el <- tableGetEntries tbl
	  mapM_ setDefaultAttr el
	where
	setDefaultAttr e
	    = do
	      n <- widgetGetName e
	      maybe (return ())
		    ( \ v1 ->
		      do
		      when (not . null $ v1)
		           (entrySetText e v1)
		    ) $ lookup n as

    tableGetAttr tbl
	= do
	  el <- tableGetEntries tbl
	  al <- mapM ( \ c ->
		       do
		       n <- widgetGetName c
		       v <- entryGetText c
		       return (n, v)
		     ) el
	  return $ filter (not . null . snd) al

tableGetEntries	:: Table -> IO [Entry]
tableGetEntries tbl
    = do
      cells   <- containerGetChildren tbl
      entries <- filterM ( \ c ->
			   do
			   n <- widgetGetName c
			   return $ NG.match ".*:.*" n
			 ) cells
      return $ map castToEntry entries

-- ------------------------------------------------------------

windowGetScreenSize	:: WindowClass self => self -> IO (Int, Int)
windowGetScreenSize win
    = do
      s <- windowGetScreen win
      w <- screenGetWidth s
      h <- screenGetHeight s
      return (w,h)

windowMoveNorthEast	:: WindowClass self => self -> IO ()
windowMoveNorthEast win
    = do
      (sw,sh) <- windowGetScreenSize win
      (w,h)   <- windowGetSize       win
      windowMove win (sw - w) 0

-- ------------------------------------------------------------

showOnStatusbar :: Statusbar -> IORef (Maybe (ContextId, MessageId)) -> String -> IO ()
showOnStatusbar sb msgRef msg
    = do
      oldRef <- readIORef msgRef			-- clear old message
      maybe (return ())
	    (uncurry (statusbarRemove sb)) $ oldRef

      cid <- statusbarGetContextId sb msg		-- show new message
      mid <- statusbarPush sb cid msg

      writeIORef msgRef $ Just (cid, mid)		-- remember message id
      updateGUI

installStatusbarLogger	:: Logger -> IO Logger
installStatusbarLogger oldLog
    = do
      msgRef <- newIORef Nothing
      withStatusbar $
        \ sb -> return (\ msg -> do
                                 oldLog msg			-- old logger remains unchanged
                                 showOnStatusbar sb msgRef msg	-- new logger is added
		       )

-- ------------------------------------------------------------
--
-- call back actions

-- ------------------------------------------------------------

loadLightbox	:: String -> [(String, String)] -> IO ()
loadLightbox path ps
    = do
      (lbw, lbt)            <- getLightbox
      widgetSetName            lbw path
      remLightboxTableContents lbt
      fillLightbox             lbt
      widgetShowAll            lbw
    where
    fillLightbox lbt
	= do
	  cols              <- get lbt tableNRows
	  rows              <- return $ ((length ps `max` 1) + cols - 1) `div` cols
	  tableResize          lbt rows cols
	  ws                <- mapM (uncurry mkToggleButton) ps
	  fillTable            lbt ws
	  resizeLightboxTable
	  widgetSetName        lbt path

-- ------------------------------------------------------------

remLightboxTableContents	:: Table -> IO ()
remLightboxTableContents lbt
    = withToggleButtons_ ( \ tb ->
			   do
			   containerRemove lbt tb
			   widgetDestroy       tb
			 ) $ lbt

-- ------------------------------------------------------------

mkLightbox	:: IO Lightbox
mkLightbox
    = do
      t                          <- tableNew 1 1 True
      widgetSetName                 t "empty lightbox table"
      w                          <- scrolledWindowNew Nothing Nothing
      widgetSetName                 w "empty lightbox"
      scrolledWindowAddWithViewport w t
      scrolledWindowSetPolicy       w PolicyAutomatic PolicyAutomatic
      return (w,t)

-- ------------------------------------------------------------

addTab		:: String -> Notebook -> IO Int
addTab lab tabs
    = do
      trcMsg $ "addTab " ++ lab
      (w, _) <- mkLightbox
      i <- notebookAppendPage tabs w lab
      trcMsg $ "addTab " ++ show i
      changeSelected $ return . (++ [[]])		-- add empty selection list
      return i

-- ------------------------------------------------------------

mkToggleButton :: String -> FilePath -> IO ToggleButton
mkToggleButton tbId tbImg
    = do
      tb <- toggleButtonNew
      vb <- vBoxNew False 3
      tl <- labelNew $ Just ""
      ic <- imageNewFromFile tbImg
      bl <- labelNew $ Just tbId

      -- pack
      containerAdd vb tl
      containerAdd vb ic
      containerAdd vb bl
      containerAdd tb vb

      -- configure labels
      labelSetSingleLineMode tl True
      setFG                  tl [StateNormal, StateActive, StatePrelight] darkRed
      set                    tl [widgetName := Just "topLabel"]
      miscSetAlignment       tl 1.0 0.5
      miscSetPadding         tl 10  0

      labelSetSingleLineMode bl True
      set                    bl [widgetName := Just "bottomLabel"]

      -- configure toggle button
      set                    tb [widgetName := Just tbId]
      setBG                  tb [StateActive]   lightRed
      setBG                  tb [StatePrelight] lightLightRed
      widgetSetSizeRequest   tb picSize picSize
      onToggled              tb $ imgButtonToggled tb

      return tb

setFG		:: WidgetClass w => w -> [StateType] -> Color -> IO ()
setFG wg sts c	= mapM_ (\ s -> widgetModifyFg wg s c) sts

setBG		:: WidgetClass w => w -> [StateType] -> Color -> IO ()
setBG wg sts c	= mapM_ (\ s -> widgetModifyBg wg s c) sts

-- ------------------------------------------------------------

switchTab	:: Int -> IO ()
switchTab i	= withTabs switchTab
    where
    switchTab tabs
	= do
	  trcMsg $ "switchTabs " ++ show i
	  setCurrTab i
	  Just w'	<- notebookGetNthPage tabs i
	  let w 	=  castToScrolledWindow w'
	  n 		<- widgetGetName w
	  tb 		<- getLbxTable w
	  tn   		<- widgetGetName tb
	  trcMsg $ "current tab (" ++ show i ++ ") contains " ++ tn
	  setLightbox 	(w, tb)
	  resizeLightboxTable
	  widgetShowAll tabs
	  return 	()

-- ------------------------------------------------------------

sortSelected	:: IO ()
sortSelected	= withLightboxTable $ withCurrSelected . sortBySelected
    where
    sortBySelected tab []
	= return ()
    sortBySelected tab xs@(x:xs1)
	= do
	  clearSelected
	  tn  <- widgetGetName     tab
	  tbs <- getToggleButtons  tab
	  ns  <- withToggleButtons
		 ( \ tb ->
		   do
		   Just n <- get tb widgetName
		   return n
		 )		   tab

	  let newns  = sortNs (zip (sortBySelect ns) [1..]) ns
	  let newtbs = map snd . sortBy (compare `F.on` fst) . zip newns $ tbs
	  trcMsg $ "sort " ++ tn ++ " new sequence: " ++ unwords (map show newns)

          execCmd ("sortpictures" : tn : map show newns)

	  withToggleButtons_ (containerRemove tab) tab
	  fillTable tab newtbs
	  return ()
	where
	sortNs :: [(String, Int)] -> [String] -> [Int]
	sortNs ns = map (fromJust . flip lookup ns)

	sortBySelect ns
	    = ps1 ++ [x] ++ reverse xs1 ++ ps2
	    where
	    (ps1', ps2') = span (/= x) ns
	    ps1		 = filter (`notElem` xs) ps1'
	    ps2          = filter (`notElem` xs) ps2'

-- ------------------------------------------------------------

storeTab	:: IO ()
storeTab
    = withLightboxTable ( \ lbt -> 
			  do
			  pn <- widgetGetName lbt
			  execCmd ["store", pn]
			  return ()
			)

-- ------------------------------------------------------------

invertSelected	:: IO ()
invertSelected
    = do
      -- trcMsg $ "invert selection"
      withLightboxTable $
        withToggleButtons_ ( \ tb ->
			     do
			     v <- toggleButtonGetActive tb
			     toggleButtonSetActive      tb (not v)
			     return ()
			   )

-- ------------------------------------------------------------

clearSelected	:: IO ()
clearSelected
    = do
      -- trcMsg $ "clear selections"
      withLightboxTable $ withToggleButtons_ (flip toggleButtonSetActive False)
      withCurrSelected updateSelected

-- ------------------------------------------------------------

withToggleButtons_	:: (ToggleButton -> IO ()) -> Table -> IO ()
withToggleButtons_ tbAction tab
    = do
      tbs <- containerGetChildren tab
      mapM_ tbAction . map castToToggleButton . reverse $ tbs


withToggleButtons	:: (ToggleButton -> IO a) -> Table -> IO [a]
withToggleButtons tbAction tab
    = do
      tbs <- containerGetChildren tab
      mapM tbAction . map castToToggleButton . reverse $ tbs

withLightboxTables	:: (Table -> IO a) -> Notebook -> IO [a]
withLightboxTables lbxAction nb
    = do
      n   <- notebookGetNPages nb
      lbs <- mapM (notebookGetNthPage nb)                             $ [0..(n-1)]
      tbs <- mapM getLbxTable . map (castToScrolledWindow . fromJust) $ lbs
      res <- mapM lbxAction   . map castToTable                       $ tbs
      return res

-- ------------------------------------------------------------

withClipboard		:: (Lightbox -> IO a) -> IO a
withClipboard cbAction	= withTabs $ withCbt
    where
    withCbt nb
	= do
	  cbw <- (notebookGetNthPage nb 0 >>= return . castToScrolledWindow . fromJust)
	  cbt <- getLbxTable cbw
	  cbAction (cbw, cbt)

withClipboardTable	:: (Table -> IO a) -> IO a
withClipboardTable cbtAction
    = withClipboard $ \ (cbw, cbt) -> cbtAction cbt


withClipboardDo		:: IO () -> IO ()
withClipboardDo act	= withTabs cbDo
    where
    cbDo nb
	= do
	  i <- notebookGetCurrentPage nb
	  notebookSetCurrentPage nb 0
	  act
	  notebookSetCurrentPage nb i
	  
-- ------------------------------------------------------------

getLbxTable	:: ScrolledWindow -> IO Table
getLbxTable w
    = do
      [vp] <- containerGetChildren w
      [tb] <- containerGetChildren (castToContainer vp)
      return (castToTable tb)

getToggleButtons	:: Table -> IO [ToggleButton]
getToggleButtons	= withToggleButtons return

getButtonNames		:: Table -> IO [String]
getButtonNames		= withToggleButtons widgetGetName

-- ------------------------------------------------------------

imgButtonToggled	:: ToggleButton -> IO ()
imgButtonToggled tb
    = do
      Just n <- get tb widgetName
      s      <- toggleButtonGetActive tb
      -- trcMsg $ "image button toggled: " ++ show n ++ " state is: " ++ show s
      changeCurrSelected ( (if s
			    then addSelected
			    else remSelected) n)
      withCurrSelected updateSelected
    where
    addSelected		:: String -> [String] -> IO [String]
    addSelected x xs	= return (x : delete x xs)

    remSelected     	:: String -> [String] -> IO [String]
    remSelected x xs   	= return  $ delete x xs

-- ------------------------------------------------------------
--
-- update all top labels after selection and deselection of images

updateSelected		:: [String] -> IO ()
updateSelected sl
    = do
      -- trcMsg $ "images selected: " ++ unwords sl
      withLightboxTable $ withToggleButtons_ (setLabel (labels sl))
    where
    labels []	= []
    labels (x:xs)	= (x, "*") : zip (reverse sl) (map show nats)

    setLabel ll tb
	= do
	  Just n <- get tb widgetName
	  let lab = maybe "" id . lookup n $ ll
	  [vb]       <- containerGetChildren (castToContainer tb)
	  [l1, _, _] <- containerGetChildren (castToContainer vb)
	  labelSetText (castToLabel l1) lab

-- ------------------------------------------------------------

fillTable	:: Table -> [ToggleButton] -> IO()
fillTable tab ws
    = do
      rows <- get tab tableNRows
      cols <- get tab tableNColumns
      mapM_ (uncurry addWidget) $ zip (coords rows cols) ws
      -- trcMsg $ "fillTable" ++ show (rows,cols)
    where
    addWidget (i, j) w
	= tableAttach tab w j (j+1) i (i+1) [] [] 2 2

    coords rows cols
	      = map (\ i -> (i `div` cols, i `mod` cols)) [0..(rows * cols - 1)]

-- ------------------------------------------------------------

lightLightRed	= Color 0xffff 0xe000 0xe000
lightRed	= Color 0xffff 0xc000 0xc000
darkRed		= Color 0x6000      0      0

picSize 	= 180
picPad  	= 2
picCols w 	= ((w - 25) `div` (picSize + 2 * picPad)) `max` 1

-- ------------------------------------------------------------

modifyTable	:: ([ToggleButton] -> IO [ToggleButton]) -> Table -> IO ()
modifyTable modifyTbs tab
    = do
      tbs <-  getToggleButtons tab
      withToggleButtons_ (containerRemove tab)  tab

      tbs' <- modifyTbs tbs
      cols <- get tab tableNColumns
      rows <- return $ ((length tbs' `max` 1) + cols - 1) `div` cols
      trcMsg $ "modifyTable " ++ show (rows, cols, length tbs')

      tableResize   tab rows cols
      fillTable     tab tbs'
      -- widgetShowAll tab
      
-- ------------------------------------------------------------

appendToggleButtons	:: [ToggleButton] -> Table -> IO ()
appendToggleButtons newTbs
    = modifyTable $ return . (++ newTbs)

setToggleButtons	:: [ToggleButton] -> Table -> IO ()
setToggleButtons newTbs
    = modifyTable $ return . const newTbs

-- ------------------------------------------------------------

resizeTable	:: Int -> Table -> IO ()
resizeTable cols tab
    = do
      oldCols <- get tab tableNColumns
      when ( oldCols /= cols) $
	   do
	   (modifyTable resize tab)
    where
    resize tbs
	= do
	  -- trcMsg $ "resizeTable cols = " ++ show cols
	  set tab [tableNColumns := cols]
	  return tbs

-- ------------------------------------------------------------

resizeLightboxTable	:: IO ()
resizeLightboxTable
    = do
      (w, h) <- withTabs widgetGetSize
      -- trcMsg $ "resizeLightboxTable: width of lightbox is " ++ show w
      withLightboxTable (resizeTable (picCols w))
      withLightboxTable widgetShowAll

-- ------------------------------------------------------------

saveAppState	:: IO ()
saveAppState	= do
		  execCmd ["close"]
		  return ()

-- ------------------------------------------------------------

quitApp		:: ResponseId -> IO ()
quitApp res	= do
		  trcMsg ("Quit application with " ++ show res)
		  evalRes res
    where
    evalRes ResponseCancel   = return ()		-- cancel quit
    evalRes ResponseNo       = withWindow mainQuit'	-- no: don't save
    evalRes ResponseYes      = do			-- yes: save
			       saveAppState
			       withWindow mainQuit'
    evalRes r                = warnMsg ("unexpected response: " ++ show r)

    mainQuit' mw
	= do
	  trcMsg "destroy main window and quit"
	  widgetDestroy mw
	  mainQuit

-- ------------------------------------------------------------

copySelectionToClipboard	:: Bool -> IO ()
copySelectionToClipboard mv
    = do
      c <- selectedContents
      b <- currTabIsClipboard
      if b
	 then warnMsg "no copy/move from clipboard to clipboard"
	 else do
	      copyToClipboard c
	      when mv $
		   removeSelected c
	      withLightbox (widgetShowAll . fst)

copySelectionFromClipboard	:: Bool -> IO ()
copySelectionFromClipboard mv
    = do
      lb@(lbw, lbt) <- getLightbox
      b <- currTabIsClipboard
      if b
	 then warnMsg "no copy/move from clipboard to clipboard"
	 else do
	      cb  <- getClipboard
	      cbt <- getToggleButtons (snd cb)
	      when (not . null $ cbt) $			-- only do something when clipboard is not empty
		   do
		   cbs <- do
			  cbs1 <- getClipboardSelected
			  if null cbs1			-- nothing in clipboard selected
			     then do			-- take whole clipboard contents
				  withClipboardDo invertSelected
				  getClipboardSelected
			     else return cbs1
		   copySelected (clipboard, reverse cbs) cb lb
		   when mv $
			withClipboardDo deleteSelectedFromClipboard
		   withClipboardDo clearSelected
		   widgetShowAll lbw

deleteSelectedFromClipboard	:: IO ()
deleteSelectedFromClipboard
    = do
      c@(pn, ns) <- selectedContents
      b <- currTabIsClipboard
      if not b
	 then warnMsg "delete only allowed in clipboard"
	 else do
	      clearSelected
	      removeSelected c
	      withLightbox (widgetShowAll . fst)

-- ------------------------------------------------------------

currTabIsClipboard	:: IO Bool
currTabIsClipboard	= withCurrTab $ return . (== 0)

copyToClipboard	::  (String, [String]) -> IO ()
copyToClipboard c
    = do
      cb <- getClipboard
      lb <- getLightbox
      copySelected  c lb cb
      clearSelected

copySelected	:: (String, [String]) -> Lightbox -> Lightbox -> IO ()
copySelected (pn, sns) srcLb@(srcLbw, srcLbt) dstLb@(dstLbw, dstLbt)
    = do
      src <- widgetGetName srcLbt
      dst <- widgetGetName dstLbt
      trcMsg $ "copy selected " ++ show (pn,sns) ++ " from " ++ src ++ " to " ++ dst
      (_, tbs) <- partitionToggleButtons
		  ( \ tb ->
		    do
		    n <- widgetGetName tb
		    return (n `elem` sns)
		  ) srcLbt

      ns   <- mapM widgetGetName              tbs
      tbs2 <- return $ sortBySelection sns ns tbs

      news <- freshNames dstLbt
      tbs3 <- mapM (uncurry $ copyTb src dst) . zip (zip sns news) $ tbs2
      appendToggleButtons (concat tbs3) dstLbt
    where
    copyTb	:: String -> String -> (String, String) -> ToggleButton -> IO [ToggleButton]
    copyTb src dst (sid, did) tb
	= do
	  [vb]       <- containerGetChildren (castToContainer tb)
	  [_, i1, _] <- containerGetChildren (castToContainer vb)
	  tbImg      <- get (castToImage i1) imageFile
	  trcMsg $ unwords ["copyTb", srcPath, dstPath, "img=", tbImg]
	  execCmd ["copypicture", srcPath, dstPath]
	  res <- mkToggleButton did (pathToIcon dstPath)
	  return [res]
	where
	srcPath = src </> sid
	dstPath = dst </> did

-- ------------------------------------------------------------

removeSelected	:: (String, [String]) -> IO ()
removeSelected sel
    = do
      lbt      <- getLightboxTable
      removeSelected' sel lbt

removeSelected'	:: (String, [String]) -> Table -> IO ()
removeSelected' (pn, sns) lbt
    = do
      (s1, s2) <- partitionToggleButtons
		  ( \ tb ->
		    do
		    n <- widgetGetName tb
		    if (n `elem` sns)
		       then isPicture (pn </> n)
		       else return False
		  ) lbt
      setToggleButtons s1 lbt
      mapM_ rmTb s2
    where
    rmTb tb
	= do
	  pid <- widgetGetName tb
	  execCmd ["removepicture", pn, pid]
	  widgetDestroy        tb

-- ------------------------------------------------------------

partitionToggleButtons	:: (ToggleButton -> IO Bool) -> Table -> IO ([ToggleButton], [ToggleButton])
partitionToggleButtons	pred tab
    = do
      tbs <- withToggleButtons
	     ( \ tb ->
	       do
	       b <- pred tb
	       return $ ( if b then Right else Left ) tb
	       ) tab
      return $ partitionEithers tbs

-- ------------------------------------------------------------

closeTab	:: IO ()
closeTab
    = do
      b <- currTabIsClipboard
      if b
	 then warnMsg "clipboard can't be closed"
	 else withTabs close
    where
    close nb
	= do
	  i  <- notebookGetCurrentPage nb
	  changeSelected (return . deleteAt i)
	  (lbw, _) <- getLightbox
	  notebookRemovePage nb i		-- this moves to another tab
	  widgetDestroy      lbw
	  widgetShowAll      nb

-- ------------------------------------------------------------

openArchive	:: IO ()
openArchive
    = do
      execCmd ["open"]
      rootPath <- execCmd1 ["pwd"]
      when (not . null $ rootPath)
	   $ do
	     openClipboard  rootPath
	     openAlbumOrPic rootPath

openClipboard	:: String -> IO ()
openClipboard path
    = do
      res <- execCmdL ["options", optionClipboard]
      cb  <- case res of
		      [_,_,w] -> return w
		      _       -> do
				 let cbp = path </> defaultClipboard
				 existsClipboard <- isAlbum cbp
				 when (not existsClipboard)
				 	  ( do
					    execCmdL ["newalbum", path, defaultClipboard]
					    return ()
					  )
				 return cbp
      cs <- listAlbumContents cb
      withTabs ( \ tabs ->
		 do
		 notebookSetCurrentPage tabs 0	-- fill clipboard tab	
		 loadLightbox cb cs
		 -- widgetShowAll tabs
		 notebookSetCurrentPage tabs 0
		 return ()
	       )
    where
    defaultClipboard = "Clipboard"
    optionClipboard  = "clipboard"

openAlbumOrPic	:: String -> IO ()
openAlbumOrPic path
    = do
      t <- isAlbum path						-- check: is it an album?
      if t
	 then do
	      allPaths <- getAllAlbumPaths
	      maybe (openAlbum path)				-- not yet loaded
		    (withTabs . flip notebookSetCurrentPage)	-- already loaded
		    $ elemIndex path allPaths
	 else openPic path
    where
    openAlbum	:: String -> IO ()
    openAlbum path
	= do
	  trcMsg $ "openAlbum " ++ path
	  cs <- listAlbumContents path
	  openTab path cs

openPic	:: String -> IO ()
openPic path
    = do
      trcMsg $ "openPic " ++ path
      im		<- imageNewFromFile ipath
      iw		<- windowNew
      containerAdd	iw im
      windowSetTitle	iw path
      widgetShowAll	iw
      windowPresent	iw
    where
    ipath	= pathToImg path

listAlbumContents	:: String -> IO [(String, String)]
listAlbumContents path
    = do
      contents <- execCmdL ["ls", path]
      names    <- return $ map fileName   contents
      paths    <- return $ map pathToIcon contents
      trcMsg $ "listAlbumContents: " ++ show (path, names)
      return (zip names paths)

openTab		:: String -> [(String, String)] -> IO ()
openTab path cs	= withTabs $ open
    where
    open tabs
	= do
	  trcMsg $ "openTab: " ++ show (path, cs)
	  n <- notebookGetNPages tabs
	  i <- addTab (fileName path) tabs
	  notebookSetCurrentPage tabs i
	  loadLightbox path cs
	  -- widgetShowAll tabs
	  notebookSetCurrentPage tabs i

-- ------------------------------------------------------------

openSelectedAlbum	:: IO ()
openSelectedAlbum
    = do
      path <- getLastSelectedPath
      if not . null $ path
	 then openAlbumOrPic path
	 else openRootAlbum
    where
    openRootAlbum
	= do
	  rootPath <- execCmd1 ["pwd"]
	  when (not . null $ rootPath) $ openAlbumOrPic rootPath

-- ------------------------------------------------------------

updateGUI	:: IO ()
updateGUI
    = do
      n <- eventsPending
      when (n > 0) ( do
		     mainIteration
		     updateGUI
		   )

-- ------------------------------------------------------------

updateAlbum		:: IO ()
updateAlbum
    = do
      lbt  <- getLightboxTable
      path <- getCurrAlbumPath
      sel  <- getLastSelectedPath
      if null sel
	 then do
	      updateEntry path		-- update album itself and all pictures
	      invertSelected
	      widgetShowAll lbt
	      updateGUI
	      updateAlbum
	 else do
	      withToggleButtons_ ( \ tb ->
				   do
				   v <- toggleButtonGetActive tb
				   when v
				        ( do
					  n <- widgetGetName    tb
                                          p <- return         $ path </> n
					  updateEntry           p
					  toggleButtonSetActive tb False
					  updateToggleButton    tb (n, pathToIcon p)
					  updateGUI
					  return ()
					)
				 ) lbt
    where
    updateEntry	:: FilePath -> IO ()
    updateEntry p
	= do
	  logMsg $ "updating " ++ p
	  execCmd ["update", p]
	  logMsg $ p ++ " up to date"

-- ------------------------------------------------------------

htmlAlbum		:: IO ()
htmlAlbum
    = do
      lbt  <- getLightboxTable
      path <- getCurrAlbumPath
      sel  <- getLastSelectedPath
      if False -- null sel
	 then do
	      htmlEntry path		-- update album itself and all pictures
	      invertSelected
	      widgetShowAll lbt
	      updateGUI
	      htmlAlbum
	 else do
	      withToggleButtons_ ( \ tb ->
				   do
				   v <- toggleButtonGetActive tb
				   when v
				        ( do
					  n <- widgetGetName    tb
                                          p <- return         $ path </> n
					  htmlEntry             p
					  toggleButtonSetActive tb False
					  updateGUI
					)
				 ) lbt
    where
    htmlEntry	:: FilePath -> IO ()
    htmlEntry p
	= do
	  logMsg $ "generting HTML for " ++ p
	  execCmd ["html", p]
	  logMsg $ "HTML generated for " ++ p

-- ------------------------------------------------------------

newSelectedAlbum	:: IO ()
newSelectedAlbum
    = do
      path <- getLastSelectedPath
      if null path
	 then warnMsg "no picture selected"
	 else do
	      isp <- isPicture path
	      if not isp
		 then warnMsg "last selected must be a picture"
		 else do
		      newName <- albumDialog
		      when (not . null $ newName)
			   ( do
			     execCmd ["makealbum", path, ";",
			              "rename",    path, newName]
			     clearSelected
			     withLightboxTable updateToggleButtons
			     return ()
			   )

-- ------------------------------------------------------------

newSelectedPic		:: IO ()
newSelectedPic
    = do
      path <- getLastSelectedPath
      if null path
	 then warnMsg "no album selected"
	 else do
	      isa <- isAlbum path
	      if not isa
		 then warnMsg "last selected must be an album"
		 else do
		      contents <- execCmd1 ["ls", path]
		      if not . null $ contents
			 then warnMsg "selected album is not empty"
			 else do
			      execCmd ["makepicture", path]
			      clearSelected

-- ------------------------------------------------------------

importPictures		:: IO ()
importPictures
    = do
      withTabs (flip notebookSetCurrentPage 0)	-- switch to clipboard
      updateGUI
      d  <- importDir
      fc <- fileChooserDialogNew
	    (Just "Import Directory Selection")
	    Nothing
	    FileChooserActionSelectFolder
	    [("Cancel", ResponseCancel), ("Import", ResponseOk)]
      ok <- fileChooserSetCurrentFolder fc d
      when ok
	   ( do
	     res <- dialogRun fc
	     fn  <- fileChooserGetFilename fc
	     trcMsg $ "importDialog " ++ show res ++ " " ++ show fn ++ " " ++ show d
	     widgetDestroy fc
	     trcMsg $ "import dir = " ++ evalRes res fn d

	     cbPath	<- getCurrAlbumPath
	     importPictures' (evalRes res fn d) cbPath
	     cbContents	<- listAlbumContents cbPath
	     loadLightbox cbPath cbContents
	     return ()
	   )
    where
    importPictures' "" _
	= return ()
    importPictures' idir path
	= do
	  execCmd ["set",	"import-dir",		idir,		";",
	           "set",	"import-since",		"1970-01-01",	";",
		   "set",	"import-dialog",	"0",		";",
	           "set",	"import-pattern",	".*",		";",
	           "import",	path					]
	  return ()

    evalRes ResponseOk (Just fn) d
	| d `isPrefixOf` fn		= drop (length d + 1) fn
    evalRes _          _         _	= ""

    importDir
	= do
	  cwd  <- getCurrentDirectory
	  idir <- do
		  d0 <- execCmd1 ["options", "import-base"]
		  return . concat . drop 2 . words $ d0
	  dir  <- canonicalizePath $ cwd </> idir
	  return dir

-- ------------------------------------------------------------

updateToggleButtons	:: Table -> IO ()
updateToggleButtons lbt
    = do
      path	<- widgetGetName	   lbt
      contents	<- listAlbumContents	   path
      tbs	<- getToggleButtons	   lbt
      mapM_ (uncurry updateToggleButton) $ zip tbs contents
      widgetShowAll                        lbt

updateToggleButton	:: ToggleButton -> (String, String) -> IO ()
updateToggleButton tb (name, img)
    = do
      set              tb [widgetName := Just name]
      [vb]	     	<- containerGetChildren (castToContainer tb)
      [_, i1, l1]	<- containerGetChildren (castToContainer vb)
      labelSetText     (castToLabel l1) name
      oldImg	<- get (castToImage i1) imageFile
      when (oldImg /= img)
	   (imageSetFromFile (castToImage i1) img)
      widgetShowAll    tb

-- ------------------------------------------------------------

loadAlbum	:: String -> IO ()
loadAlbum n	= loadLightbox n imageDescr

imageDescr	= zip imgNames ( map imgFile $ imgNames )
		  where
		  imgNames = map ( ("pic-" ++)
				   . reverse . take 4 . reverse
				   . ("000" ++)
				   . show
				 )
				 $ [(1::Int)..21]
		  imgFile  = ("160x120/Hagenbeck/Zebras/" ++) . (++ ".jpg")

-- ------------------------------------------------------------

nameGen		:: [String] -> [String]
nameGen inUse	= filter (`notElem` inUse) . names $ nats
		  where
		  names = map ( ("pic-" ++)
				   . reverse . take 4 . reverse
				   . ("000" ++)
				   . show
				 )

freshNames	:: Table -> IO [String]
freshNames tab
    = do
      inUse <- withToggleButtons widgetGetName tab
      return $ nameGen inUse

-- ------------------------------------------------------------

lightboxContents	:: Notebook -> IO [(String, [String])]
lightboxContents	= withLightboxTables listTab

currLightboxContents	:: IO (String, [String])
currLightboxContents	= withLightboxTable listTab

selectedContents	:: IO (String, [String])
selectedContents	= withLightboxTable $ withCurrSelected . selected
    where
    selected lbt sels
	= do
	  pn <- widgetGetName lbt
	  return (pn, reverse sels)

listTab 		:: Table -> IO (String, [String])
listTab lbt
    = do
      pn <-                   widgetGetName lbt
      ns <- withToggleButtons widgetGetName lbt
      return (pn, ns)

-- ------------------------------------------------------------

pathToIcon	:: String -> String
pathToIcon	= ("160x120" ++) . (++ ".jpg")

pathToImg	:: String -> String
pathToImg	= ("1400x1050" ++) . (++ ".jpg")

-- ------------------------------------------------------------

listLightboxContents 	:: IO ()
listLightboxContents	= withTabs $ (\ lbt -> lightboxContents lbt >>= logMsg . show )

listSelectedContents :: IO ()
listSelectedContents    = selectedContents >>= logMsg . show

testOP = do
	 listLightboxContents
	 withSelected $ logMsg . ("selection= " ++ ) . show
	 withCurrTab  $ logMsg . ("currTab=   " ++) . show

testOP2
    = do
      cmd <- albumDialog
      execCmd $ words cmd
      return ()          

testOP1
    = testOP2

-- ------------------------------------------------------------

editAttributes	:: IO ()
editAttributes
    = do
      path  <- getLastSelectedPath
      as    <- getAttributes path
      attrs <- attrDialog as
      trcMsg $ "attribute dialog, result: " ++ show attrs
      setSelectedAttrs as attrs
      return ()

setSelectedAttrs	:: Attrs -> Attrs -> IO ()
setSelectedAttrs asOld as
    = do
      lbt  <- getLightboxTable
      path <- getCurrAlbumPath
      withToggleButtons_
        ( \ tb ->
	  do
	  v <- toggleButtonGetActive tb
	  when v
	    ( do
	      n <- widgetGetName    tb
              p <- return         $ path </> n
	      updateAttrs           p
	      toggleButtonSetActive tb False
	      updateGUI
	    )
	) lbt
    where
    updateAttrs p
	= mapM_ (uncurry update1Attr) as
	where
	setAttr k v
	    = do
	      execCmd $ ["attr", p, k] ++ (if null v then [] else [v])
	      return ()
	update1Attr k v
	    | null v
		= return ()
	    | v == "\"\""					-- delete an attribute
		= setAttr k ""
	    | k == show keyKeywords && ("+" `isPrefixOf` v)	-- merge keywords
		= do
		  oldAs <- getAttributes p
		  setAttr k $ mergeAttrs (tail v) (fromMaybe "" . lookup k $ oldAs)
	    | otherwise						-- set an Attribute
		= setAttr k v
	mergeAttrs as
	    = unwords . sort . nub . (words as ++) . words

-- ------------------------------------------------------------

getAttributes	:: String -> IO Attrs
getAttributes path
    = do
      if null path
	 then return []
	 else do
	      es <- execCmd ["cat", path]
	      er <- readXmlVal AT.xpAlbumEntry es
	      let res = toAttrs er
	      trcMsg $ "getAttributes: " ++ show res
	      return res
    where
    toAttrs
	= maybe AT.emptyAttrs (AT.picAttrs . snd)
	  >>>
	  M.toList
          >>>
	  map (first show)
	  >>>
	  map (second (concat . T.runLA (T.xshow T.xread)))
	  
-- ------------------------------------------------------------

readXmlVal	:: T.PU a -> String -> IO (Maybe a)
readXmlVal pk inp
    = do
      s <- T.runX ( T.readString [(T.a_remove_whitespace, T.v_1)
				 ,(T.a_validate,          T.v_0)
				 ] inp
		  >>>
		  T.xunpickleVal pk
		)
      return $ listToMaybe s

-- ------------------------------------------------------------
--
-- auxiliary list ops

deleteAt		:: Int -> [a] -> [a]
deleteAt _ []		= []
deleteAt 0 xs    	= tail xs
deleteAt i (x:xs)	= x : deleteAt (i-1) xs

setAt			:: Int -> a -> [a] -> [a]
setAt _ _ []		= []
setAt 0 v xs    	= v : tail xs
setAt i v (x:xs)	= x : setAt (i-1) v xs

sortBySelection 	:: (Eq a) => [a] -> [a] -> [b] -> [b]
sortBySelection sns ns
    = map snd . sortBy (compare `F.on` fst) . zip ixseq
    where
    ixmap = zip sns nats
    ixseq = map (fromJust . flip lookup ixmap) ns

-- ------------------------------------------------------------
--
-- model calling functions

execCmd		:: [String] -> IO String
execCmd ws	= do
		  trcMsg $ "exec cmd: " ++ cmd
		  m0        <- getModel
		  log       <- getLogger
		  (res, m1) <- execModel log cmd m0
		  setModel m1
		  trcMsg $ "exec res:\n" ++ res
		  return res
		where
		cmd = unwords ws

execCmdL	:: [String] -> IO [String]
execCmdL ws	= execCmd ws >>= return . words

execCmd1	:: [String] -> IO String
execCmd1 ws	= execCmdL ws >>= return . unwords

isAlbum		:: String -> IO Bool
isAlbum	path	= do
		  t <- execCmd1 ["isalbum", path]
		  return (t == "True")

isPicture		:: String -> IO Bool
isPicture path	= do
		  t <- execCmd1 ["isalbum", path]
		  return (t == "False")

quitM		:: IO String
quitM		= execCmd ["exit"]

saveAndQuitM	:: IO String
saveAndQuitM	= execCmd ["close", ";", "exit"]

-- ------------------------------------------------------------
