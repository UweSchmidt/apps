// $Id: Opcodes.java,v 1.8 2007-01-11 21:29:59 uwe Exp $

package ppl;

interface Opcodes {

    static final int
	locAddr 		= 0
	, absAddr 		= locAddr + 1;
	

    static final int
	addi			= 1
        , subi			= addi + 1
	, muli			= subi + 1
	, divi			= muli + 1
	, modi			= divi + 1
	, maxi			= modi + 1
	, mini			= maxi + 1
	, eqi			= mini + 1
	, gei			= eqi + 1
	, gti			= gei + 1
	, incri			= gti + 1
	, decri			= incri + 1
	, addf			= decri + 1
	, subf			= addf + 1
	, mulf			= subf + 1
	, divf			= mulf + 1
	, maxf			= divf + 1
	, minf			= maxf + 1
	, eqf			= minf + 1
	, gef			= eqf + 1
	, gtf			= gef + 1
	, i2s			= gtf + 1
	, f2s			= i2s + 1
	, i2f			= f2s + 1
	, trunc			= i2f + 1
	, round			= trunc + 1
	, concs			= round + 1
	, isemptyl		= concs + 1
	, lengthl		= isemptyl + 1
	, taill			= lengthl + 1
	, concl			= taill + 1
	, consl		        = concl + 1
	, appendl		= consl + 1
	, indexl		= appendl + 1
	, width			= indexl + 1
	, height		= width + 1
	, black			= height + 1
	, white			= black + 1
	, grey			= white + 1
	, gamma			= grey + 1
	, invert		= gamma + 1
	, bitmap		= invert + 1
	, blackAndWhite		= bitmap + 1
	, reduceColor		= blackAndWhite + 1
	, flipHorizontal	= reduceColor + 1
	, flipVertical		= flipHorizontal + 1
	, flipDiagonal		= flipVertical + 1
	, rotate		= flipDiagonal + 1
	, shift			= rotate + 1
	, cut			= shift + 1
	, paste			= cut + 1
	, scale			= paste + 1
	, shrink		= scale + 1
	, replicate		= shrink + 1
	, resize		= replicate + 1
	, sideBySide		= resize + 1
	, above			= sideBySide + 1
        , partitionHorizontal	= above + 1
        , partitionVertical	= partitionHorizontal + 1
	, splitHorizontal	= partitionVertical + 1
	, splitVertical		= splitHorizontal + 1
	, mergeHorizontal	= splitVertical + 1
	, mergeVertical		= mergeHorizontal + 1
        , concatHorizontal      = mergeVertical + 1
        , concatVertical        = concatHorizontal + 1
	, mean			= concatVertical + 1
	, diff			= mean + 1
	, inverseMean		= diff + 1
	, inverseDiff		= inverseMean + 1
	, mulp			= inverseDiff + 1
	, maxp			= mulp + 1
	, minp			= maxp + 1
	, terminate		= minp + 1
	, abort			= terminate + 1
	;

    static final int
        store 			= 1
	, load			= store + 1
	, write			= load + 1
	, writeln		= write + 1
	, getArgs		= writeln + 1
        , dump                  = getArgs + 1
	;
}
