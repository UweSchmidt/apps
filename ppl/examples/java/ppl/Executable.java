// $Id: Executable.java,v 1.4 2007-01-11 21:29:59 uwe Exp $

package ppl;

import ppl.Instr;
import ppl.MachineValue;
import ppl.Opcodes;

public
    class Executable implements Opcodes {

	public final
	    Instr [] code;

	public final
	    MachineValue [] globalDataSegment;

	public
	    Executable(Instr [] code,
		       int dataSegmentLen
		       ) {
	    this.code = code;
	    this.globalDataSegment = new MachineValue [dataSegmentLen];
	    // ... maybe incomplete ...
	}

	public static
	    Instr loadi(int value) {
	    // ... incomplete ...
	    return null;
	}

	public static
	    Instr loadf(double value) {
	    // ... incomplete ...
	    return null;
	}

	public static
	    Instr loads(String value) {
	    // ... incomplete ...
	    return null;
	}

	public static
	    Instr undef() {
	    // ... incomplete (???) ...
	    return null;
	}

	public static
	    Instr emptyl() {
	    // ... incomplete ...
	    return null;
	}

	public static
	    Instr load(int addrType,
			     int offset) {
	    // ... incomplete ...
	    return null;
	}

	public static
	    Instr dup() {
	    // ... incomplete ...
	    return null;
	}

	public static
	    Instr store(int addrType,
			      int offset) {
	    // ... incomplete ...
	    return null;
	}

	public static
	    Instr pushj(int displ) {
	    // ... incomplete ...
	    return null;
	}

	public static
	    Instr pop() {
	    // ... incomplete ...
	    return null;
	}

	public static
	    Instr popj() {
	    // ... incomplete ...
	    return null;
	}

	public static
	    Instr compute(int opcode) {
	    // ... incomplete ...
	    return null;
	}

	public static
	    Instr svc(int subroutine) {
	    // ... incomplete ...
	    return null;
	}

	public static
	    Instr entry(int len) {
	    // ... incomplete ...
	    return null;
	}

	public static
	    Instr exit() {
	    // ... incomplete ...
	    return null;
	}

	public static
	    Instr branch(boolean cond,
			       int disp) {
	    // ... incomplete ...
	    return null;
	}

	public static
	    Instr jump(int displ) {
	    // ... incomplete ...
	    return null;
	}

	public static
	    Instr illegalInstr(String error) {
	    // ... incomplete ...
	    return null;
	}

    }
