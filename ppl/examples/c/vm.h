/* $Id: pplvm.h,v 1.2 2007-01-11 21:29:59 uwe Exp $ */

typedef int Int;

typedef float Real;

typedef enum {   op_loadi
		 , op_loadf
		 , op_loads
		 , op_undef
		 , op_emptyl
		 , op_loadLoc
		 , op_loadAbs
		 , op_storeLoc
		 , op_storeAbs
		 , op_pop
		 , op_dup
		 , op_pushj
		 , op_popj
		 , op_entry
		 , op_exit
		 , op_brTrue
		 , op_brFalse
		 , op_jump
		 , op_illegalInstr

		 , op_above
		 , op_addf
		 , op_addi
		 , op_appendl
		 , op_bitmap
		 , op_black
		 , op_blackAndWhite
		 , op_concl
		 , op_concs
		 , op_cut
		 , op_diff
		 , op_divf
		 , op_divi
		 , op_eqi
		 , op_flipDiagonal
		 , op_flipHorizontal
		 , op_flipVertical
		 , op_gamma
		 , op_gei
		 , op_getArgs
		 , op_grey
		 , op_gti
		 , op_height
		 , op_i2s
		 , op_indexl
		 , op_inverseDiff
		 , op_inverseMean
		 , op_invert
		 , op_isemptyl
		 , op_lengthl
		 , op_maxi
		 , op_maxp
		 , op_mean
		 , op_mergeHorizontal
		 , op_mergeVertical
		 , op_mini
		 , op_minp
		 , op_modi
		 , op_mulf
		 , op_muli
		 , op_mulp
		 , op_partitionHorizontal
		 , op_partitionVertical
		 , op_paste
		 , op_consl
		 , op_reduceColor
		 , op_replicate
		 , op_resize
		 , op_rotate
		 , op_scale
		 , op_shift
		 , op_sideBySide
		 , op_splitHorizontal
		 , op_subf
		 , op_subi
		 , op_svc_exit
		 , op_svc_getArgs
		 , op_svc_load
		 , op_svc_store
		 , op_svc_write
		 , op_svc_writeln
		 , op_taill
		 , op_white
		 , op_width
} Opcode;

typedef struct {
    Opcode op;
    void * args;
} Instr;

extern int main_pplvm(int argc, char * argv[], Instr * instructions, int dataSegmentLength);
