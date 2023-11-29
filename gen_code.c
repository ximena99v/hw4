// gen_code module uses the literal table to track numeric literals used in the program, 
// and to lay these out in the binary object file's data section

/* $Id: gen_code.h,v 1.10 2023/11/15 06:43:49 leavens Exp $ */
#ifndef _GEN_CODE_H
#define _GEN_CODE_H
#include <stdio.h>
#include "ast.h"
#include "bof.h"
#include "instruction.h"
#include "code.h"
#include "gen_code.h"

#define STACK_SPACE 4096

// Initialize the code generator
void gen_code_initialize()
{
    literal_table_initialize();

}

// Requires: bf if open for writing in binary
// and prior to this scope checking and type checking have been done.
// Write all the instructions in cs to bf in order
static void gen_code_output_seq(BOFFILE bf, code_seq cs)
{
    while (!code_seq_is_empty(cs)) {
	bin_instr_t inst = code_seq_first(cs)->instr;
	instruction_write_bin_instr(bf, inst);
	cs = code_seq_rest(cs);
    }
}

// Return a header appropriate for the give code
static BOFHeader gen_code_program_header(code_seq main_cs)
{
    BOFHeader ret;
    strncpy(ret.magic, "FBF", 4);  // for FLOAT SRM
    ret.text_start_address = 0;
    // remember, the unit of length in the BOF format is a byte!
    ret.text_length = code_seq_size(main_cs) * BYTES_PER_WORD;
    int dsa = MAX(ret.text_length, 1024);
    ret.data_start_address = dsa;
    ret.ints_length = 0; // FLOAT has no int literals
    ret.floats_length = literal_table_size() * BYTES_PER_WORD;
    int sba = dsa
	+ ret.data_start_address
	+ ret.ints_length + ret.floats_length + STACK_SPACE;
    ret.stack_bottom_addr = sba;
    return ret;
}

static void gen_code_output_literals(BOFFILE bf)
{
    literal_table_start_iteration();
    while (literal_table_iteration_has_next()) {
	float_type w = literal_table_iteration_next();
	// debug_print("Writing literal %f to BOF file\n", w);
	bof_write_float(bf, w);
    }
    literal_table_end_iteration(); // not necessary
}

// Requires: bf is open for writing in binary
// Write the program's BOFFILE to bf
static void gen_code_output_program(BOFFILE bf, code_seq main_cs)
{
    BOFHeader bfh = gen_code_program_header(main_cs);
    bof_write_header(bf, bfh);
    gen_code_output_seq(bf, main_cs);
    gen_code_output_literals(bf);
    bof_close(bf);
}

// Requires: bf if open for writing in binary
// Generate code for prog into bf
/* the function gen_code_program walks the program's AST and 
uses the BOFFILE it receives to write the program's code into the BOF file,
 which can then be executed by the SRM*/
void gen_code_program(BOFFILE bf, block_t prog)
{
    code_seq main_cs;
    // We want to make the main program's AR look like all blocks... so:
    // allocate space and initialize any variables
    main_cs = gen_code_var_decls(prog.var_decls);
    int vars_len_in_bytes
	= (code_seq_size(main_cs) / 2) * BYTES_PER_WORD;
    // there is no static link for the program as a whole,
    // so nothing to do for saving FP into A0 as would be done for a block
    main_cs = code_seq_concat(main_cs, code_save_registers_for_AR());
    main_cs = code_seq_concat(main_cs, gen_code_stmt(prog.stmt));
    main_cs = code_seq_concat(main_cs, code_restore_registers_from_AR());
    main_cs = code_seq_concat(main_cs, code_deallocate_stack_space(vars_len_in_bytes));
    main_cs = code_seq_add_to_end(main_cs, code_exit());
    gen_code_output_program(bf, main_cs);

}

// Requires: bf if open for writing in binary
// Generate code for the given AST
code_seq gen_code_block(block_t blk)
{

}

// Generate code for the const-decls, cds
// There are 3 instructions generated for each identifier declared
// (one to allocate space and two to initialize that space)
code_seq gen_code_const_decls(const_decls_t cds)
{

}

// Generate code for the const-decl, cd
code_seq gen_code_const_decl(const_decl_t cd)
{
    return gen_code_idents(cdf.idents, cdf.type);

}

// Generate code for the const-defs, cdfs
code_seq gen_code_const_defs(const_defs_t cdfs)
{

}

// Generate code for the const-def, cdf
code_seq gen_code_const_def(const_def_t cdf)
{
    
}

// Generate code for the var_decls_t vds to out
// There are 2 instructions generated for each identifier declared
// (one to allocate space and another to initialize that space)
code_seq gen_code_var_decls(var_decls_t vds)
{
    code_seq ret = code_seq_empty();
    var_decl_t *vdp = vds.var_decls;
    while (vdp != NULL) {
	// generate these in reverse order,
	// so the addressing offsets work properly
	ret = code_seq_concat(gen_code_var_decl(*vdp), ret);
	vdp = vdp->next;
    }
    return ret;

}

// Generate code for a single <var-decl>, vd,
// There are 2 instructions generated for each identifier declared
// (one to allocate space and another to initialize that space)
code_seq gen_code_var_decl(var_decl_t vd)
{
    return gen_code_idents(vd.idents, vd.type);
}

// Generate code for the identififers in idents
// in reverse order (so the first declared are allocated last).
// There are 2 instructions generated for each identifier declared
// (one to allocate space and another to initialize that space)
code_seq gen_code_idents(idents_t idents)
{

}

// (Stub for:) Generate code for the procedure declarations
code_seq gen_code_proc_decls(proc_decls_t pds)
{

}

// (Stub for:) Generate code for a procedure declaration
code_seq gen_code_proc_decl(proc_decl_t pd)
{
    return gen_code_idents(pd.idents, pd.type);

}

// Generate code for stmt
code_seq gen_code_stmt(stmt_t stmt)
{

}

// Generate code for stmt
code_seq gen_code_assign_stmt(assign_stmt_t stmt)
{

}

// Generate code for stmt
code_seq gen_code_call_stmt(call_stmt_t stmt)
{

}


// Generate code for stmt
code_seq gen_code_begin_stmt(begin_stmt_t stmt)
{

}

// Generate code for the list of statments given by stmts
code_seq gen_code_stmts(stmts_t stmts)
{

}

// Generate code for the if-statment given by stmt
code_seq gen_code_if_stmt(if_stmt_t stmt)
{

}

// Generate code for the if-statment given by stmt
code_seq gen_code_while_stmt(while_stmt_t stmt)
{

}

// Generate code for the read statment given by stmt
code_seq gen_code_read_stmt(read_stmt_t stmt)
{
    // put number read into $v0
    code_seq ret = code_seq_singleton(code_rch());
    // put frame pointer from the lexical address of the name
    // (using stmt.idu) into $t9
    assert(stmt.idu != NULL);
    ret = code_seq_concat(ret, code_compute_fp(T9, stmt.idu->levelsOutward));
    assert(id_use_get_attrs(stmt.idu) != NULL);
    unsigned int offset_count = id_use_get_attrs(stmt.idu)->offset_count;
    assert(offset_count <= USHRT_MAX); // it has to fit!
    ret = code_seq_add_to_end(ret,
			      code_seq_singleton(code_fsw(T9, V0, offset_count)));
    return ret;

}

// Generate code for the write statment given by stmt.
code_seq gen_code_write_stmt(write_stmt_t stmt)
{
     // put the result into $a0 to get ready for PCH
    code_seq ret = gen_code_expr(stmt.expr);
    ret = code_seq_concat(ret, code_pop_stack_into_reg(A0));
    ret = code_seq_add_to_end(ret, code_pint());
    return ret;


}

// Generate code for the skip statment, stmt
code_seq gen_code_skip_stmt(skip_stmt_t stmt)
{
    // never happens, but suppresses a warning from gcc
    return code_seq_empty();

}

// Requires: reg != T9
// Generate code for cond, putting its truth value
// on top of the runtime stack
// and using V0 and AT as temporary registers
// May modify HI,LO when executed
code_seq gen_code_condition(condition_t cond)
{
    switch (cond.cond_kind) {
    case cond_odd_cond: // odd condition
	return gen_code_odd_cond(cond.data.odd_cond);
	break;
    case cond_rel_op: // relational operator condition
	return en_code_relop_cond(cond.data.rel_op_cond);
	break;
    }
    // never happens, but suppresses a warning from gcc
    return code_seq_empty();
    
}

// Generate code for cond, putting its truth value
// on top of the runtime stack
// and using V0 and AT as temporary registers
// Modifies SP, HI,LO when executed
code_seq gen_code_odd_condition(odd_condition_t cond)
{
    code_seq ret = gen_code_expr(cond.expr);
    ret = code_seq_concat(ret, code_pop_stack_into_reg(V0));
    ret = code_seq_add_to_end(ret, code_seq_singleton(code_andi(V0, V0, 1)));
    ret = code_seq_add_to_end(ret, code_push_reg(V0));
    return ret;
}

// Generate code for cond, putting its truth value
// on top of the runtime stack
// and using V0 and AT as temporary registers
// May also modify SP, HI,LO when executed
code_seq gen_code_rel_op_condition(rel_op_condition_t cond)
{
    // load top of the stack (the second operand) into AT
    code_seq ret = code_pop_stack_into_reg(AT, typ);
    // load next element of the stack into V0
    ret = code_seq_concat(ret, code_pop_stack_into_reg(V0, typ));

    // start out by doing the comparison
    // and skipping the next 2 instructions if it's true
    code_seq do_op = code_seq_empty();
    switch (rel_op.code) {
    case eqsym: 
	if (typ == float_te) {
	    do_op = code_seq_singleton(code_bfeq(V0, AT, 2));
	} else {
	    do_op = code_seq_singleton(code_beq(V0, AT, 2));
	}
	break;
    case neqsym:
    return ret;
}

// Generate code for the rel_op
// applied to 2nd from top and top of the stack,
// putting the result on top of the stack in their place,
// and using V0 and AT as temporary registers
// May also modify SP, HI,LO when executed
code_seq gen_code_rel_op(token_t rel_op)
{
    switch (rel_op) {
    case token_eq: // equal
    return code_seq_singleton(code_seq_singleton);
    break;
    case token_neq: // not equal
    return code_seq_singleton(code_seq_singleton);
    break;
    case token_lt: // less than
    return code_seq_singleton(code_seq_singleton);
    break;
    case token_gt: // greater than
    return code_seq_singleton(code_seq_singleton);
    break;
    case token_leq: // less than or equal
    return code_seq_singleton(code_seq_singleton);
    break;
    case token_geq: // greater than or equal
    return code_seq_singleton(code_seq_singleton);
    break;
    }
    
    // never happens, but suppresses a warning from gcc
    return code_seq_empty();

}

// Generate code for the expression exp
// putting the result on top of the stack,
// and using V0 and AT as temporary registers
// May also modify SP, HI,LO when executed
code_seq gen_code_expr(expr_t exp)
{
    switch (exp.expr_kind) {
    case expr_bin_op:
	return gen_code_binary_op_expr(exp.data.binary);
	break;
    case expr_ident:
	return gen_code_ident(exp.data.ident);
	break;
    case expr_number:
	return gen_code_number(exp.data.number);
	break;
    case expr_logical_not:
	return gen_code_logical_not_expr(*(exp.data.logical_not));
	break;
    default:
	bail_with_error("Unexpected expr_kind_e (%d) in gen_code_expr",
			exp.expr_kind);
	break;
    }
    // never happens, but suppresses a warning from gcc
    return code_seq_empty();

}

// Generate code for the expression exp
// putting the result on top of the stack,
// and using V0 and AT as temporary registers
// May also modify SP, HI,LO when executed
code_seq gen_code_binary_op_expr(binary_op_expr_t exp);

// Generate code to apply arith_op to the
// 2nd from top and top of the stack,
// putting the result on top of the stack in their place,
// and using V0 and AT as temporary registers
// May also modify SP, HI,LO when executed
code_seq gen_code_arith_op(token_t arith_op);

// Generate code to put the value of the given identifier
// on top of the stack
// Modifies T9, V0, and SP when executed
code_seq gen_code_ident(ident_t id)
{
    /* design:
       [code to load fp for the variable]
       LOD [offset for the variable]
     */
    id_use *idu = id->data.ident.idu;
    lexical_address *la = lexical_address_create(idu->levelsOutward,
						 idu->attrs->loc_offset);
    return code_load_from_lexical_address(la);
}

// Generate code to put the given number on top of the stack
code_seq gen_code_number(number_t num)
{
    return code_seq_singleton(code_lit(word2float(num->data.number.value)));

}

