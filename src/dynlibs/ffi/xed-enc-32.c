/*BEGIN_LEGAL 
Intel Open Source License 

Copyright (c) 2002-2013 Intel Corporation. All rights reserved.
 
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

Redistributions of source code must retain the above copyright notice,
this list of conditions and the following disclaimer.  Redistributions
in binary form must reproduce the above copyright notice, this list of
conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.  Neither the name of
the Intel Corporation nor the names of its contributors may be used to
endorse or promote products derived from this software without
specific prior written permission.
 
THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE INTEL OR
ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
END_LEGAL */
    
/// @file xed-ex5-enc.c

// encoder example.

/* compile this with -DXED_DECODER to enable the decoder functionality.

*/

#include "xed-interface.h"
#include "xed-examples-util.h"

#include <stdio.h>
#include <stdlib.h> // malloc, etc.
#include <assert.h> 
#include <errno.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>
#include <signal.h>

#include "mosml/mlvalues.h"
#include "mosml/alloc.h"
#include "mosml/memory.h"
#include "mosml/fail.h"
#include "mosml/str.h"

value xed_enc_constants(value unit) {
  value res = alloc_tuple(10);

  Field(res, 0) = Val_long(XED_MAX_INSTRUCTION_BYTES);
  Field(res, 1) = Val_long(XED_MACHINE_MODE_LEGACY_32);
  Field(res, 2) = Val_long(XED_ADDRESS_WIDTH_32b);
  Field(res, 3) = Val_long(XED_MACHINE_MODE_LONG_64);
  Field(res, 4) = Val_long(XED_SYNTAX_INTEL);
  Field(res, 5) = Val_long(XED_SYNTAX_ATT);
  Field(res, 6) = Val_long(sizeof(xed_encoder_instruction_t));
  Field(res, 7) = Val_long(sizeof(xed_encoder_request_t));
  Field(res, 8) = Val_long(sizeof(xed_decoded_inst_t));
  Field(res, 9) = Val_long(sizeof(xed_state_t));

  return res;
}

xed_enc_displacement_t xed_disp_(xed_uint32_t   l,
                                 xed_uint32_t   h,
                                 xed_uint32_t   displacement_width) {
    xed_enc_displacement_t x;
    x.displacement =  h * 0x100000000 + l;
    x.displacement_width = displacement_width;
    return x;
}

xed_encoder_operand_t xed_relbr_(xed_int32_t brdisp,
                                 xed_uint_t width) {
    xed_encoder_operand_t o;
    o.type = XED_ENCODER_OPERAND_TYPE_BRDISP;
    o.u.brdisp = brdisp;
    o.width = width;
    return o;
}

 xed_encoder_operand_t xed_ptr_(xed_int32_t brdisp,
                                xed_uint_t width) {
    xed_encoder_operand_t o;
    o.type = XED_ENCODER_OPERAND_TYPE_PTR;
    o.u.brdisp = brdisp;
    o.width = width;
    return o;
}

xed_encoder_operand_t xed_reg_(xed_reg_enum_t reg) {
    xed_encoder_operand_t o;
    o.type = XED_ENCODER_OPERAND_TYPE_REG;
    o.u.reg = reg;
    o.width = 0;
    return o;
}

xed_encoder_operand_t xed_imm0_(xed_uint32_t l,
                                xed_uint32_t h,
                                xed_uint_t width) {
    xed_encoder_operand_t o;
    o.type = XED_ENCODER_OPERAND_TYPE_IMM0;
    o.u.imm0 = h * 0x100000000 + l;
    o.width = width;
    return o;
}

xed_encoder_operand_t xed_simm0_(xed_int32_t v,
                                 xed_uint_t width) {
    xed_encoder_operand_t o;
    o.type = XED_ENCODER_OPERAND_TYPE_SIMM0;
    /* sign conversion: we store the int32 in an uint64. It gets sign
    extended.  Later we convert it to the right width for the
    instruction. The maximum width of a signed immediate is currently
    32b. */
    o.u.imm0 = v;
    o.width = width;
    return o;
}

xed_encoder_operand_t xed_imm1_(xed_uint8_t v) {
    xed_encoder_operand_t o;
    o.type = XED_ENCODER_OPERAND_TYPE_IMM1;
    o.u.imm1 = v; 
    o.width = 8;
    return o;
}

xed_encoder_operand_t xed_other_(xed_operand_enum_t operand_name,
                                 xed_int32_t value) {
    xed_encoder_operand_t o;
    o.type = XED_ENCODER_OPERAND_TYPE_OTHER;
    o.u.s.operand_name = operand_name;
    o.u.s.value = value;
    o.width = 0;
    return o;
}

xed_encoder_operand_t xed_seg0_(xed_reg_enum_t seg0) {
    xed_encoder_operand_t o;
    o.type = XED_ENCODER_OPERAND_TYPE_SEG0;
    o.u.reg = seg0;
    return o;
}

xed_encoder_operand_t xed_seg1_(xed_reg_enum_t seg1) {
    xed_encoder_operand_t o;
    o.type = XED_ENCODER_OPERAND_TYPE_SEG1;
    o.u.reg = seg1;
    return o;
}

xed_encoder_operand_t xed_mem_b_(xed_reg_enum_t base,
                                 xed_uint_t width) {
    xed_encoder_operand_t o;
    o.type = XED_ENCODER_OPERAND_TYPE_MEM;
    o.u.mem.base = base;
    o.u.mem.seg = XED_REG_INVALID;
    o.u.mem.index= XED_REG_INVALID;
    o.u.mem.scale = 0;
    o.u.mem.disp.displacement = 0;
    o.u.mem.disp.displacement_width = 0;
    o.width = width;
    return o;
}

xed_encoder_operand_t xed_mem_bd_(xed_reg_enum_t base, 
                                  xed_enc_displacement_t disp,
                                  xed_uint_t width) {
    xed_encoder_operand_t o;
    o.type = XED_ENCODER_OPERAND_TYPE_MEM;
    o.u.mem.base = base;
    o.u.mem.seg = XED_REG_INVALID;
    o.u.mem.index= XED_REG_INVALID;
    o.u.mem.scale = 0;
    o.u.mem.disp =disp;
    o.width = width;
    return o;
}

xed_encoder_operand_t xed_mem_bisd_(xed_reg_enum_t base, 
                                    xed_reg_enum_t index, 
                                    xed_uint_t scale,
                                    xed_enc_displacement_t disp,
                                    xed_uint_t width) {
    xed_encoder_operand_t o;
    o.type = XED_ENCODER_OPERAND_TYPE_MEM;
    o.u.mem.base = base;
    o.u.mem.seg = XED_REG_INVALID;
    o.u.mem.index= index;
    o.u.mem.scale = scale;
    o.u.mem.disp = disp;
    o.width = width;
    return o;
}

xed_encoder_operand_t xed_mem_gb_(xed_reg_enum_t seg,
                                  xed_reg_enum_t base,
                                  xed_uint_t width) {
    xed_encoder_operand_t o;
    o.type = XED_ENCODER_OPERAND_TYPE_MEM;
    o.u.mem.base = base;
    o.u.mem.seg = seg;
    o.u.mem.index= XED_REG_INVALID;
    o.u.mem.scale = 0;
    o.u.mem.disp.displacement = 0;
    o.u.mem.disp.displacement_width = 0;
    o.width = width;
    return o;
}

xed_encoder_operand_t xed_mem_gbd_(xed_reg_enum_t seg,
                                   xed_reg_enum_t base, 
                                   xed_enc_displacement_t disp,
                                   xed_uint_t width) {
    xed_encoder_operand_t o;
    o.type = XED_ENCODER_OPERAND_TYPE_MEM;
    o.u.mem.base = base;
    o.u.mem.seg = seg;
    o.u.mem.index= XED_REG_INVALID;
    o.u.mem.scale = 0;
    o.u.mem.disp = disp;
    o.width = width;
    return o;
}

xed_encoder_operand_t xed_mem_gd_(xed_reg_enum_t seg,
                                  xed_enc_displacement_t disp,
                                  xed_uint_t width) {
    xed_encoder_operand_t o;
    o.type = XED_ENCODER_OPERAND_TYPE_MEM;
    o.u.mem.base = XED_REG_INVALID;
    o.u.mem.seg = seg;
    o.u.mem.index= XED_REG_INVALID;
    o.u.mem.scale = 0;
    o.u.mem.disp = disp;
    o.width = width;
    return o;
}

xed_encoder_operand_t xed_mem_gbisd_(xed_reg_enum_t seg, 
                                     xed_reg_enum_t base, 
                                     xed_reg_enum_t index, 
                                     xed_uint_t scale,
                                     xed_enc_displacement_t disp, 
                                     xed_uint_t width) {
    xed_encoder_operand_t o;
    o.type = XED_ENCODER_OPERAND_TYPE_MEM;
    o.u.mem.base = base;
    o.u.mem.seg = seg;
    o.u.mem.index= index;
    o.u.mem.scale = scale;
    o.u.mem.disp = disp;
    o.width = width;
    return o;
}

void xed_addr_(xed_encoder_instruction_t* x, 
                                xed_uint_t width) {
    x->effective_address_width = width;
}


void xed_rep_(xed_encoder_instruction_t* x) { 
    x->prefixes.s.rep=1;
}

void xed_repne_(xed_encoder_instruction_t* x) { 
    x->prefixes.s.repne=1;
}

void xed_lock_(xed_encoder_instruction_t* x) { 
    x->prefixes.s.lock=1;
}

void xed_inst_(
    xed_encoder_instruction_t* inst,
    xed_state_t mode,
    xed_iclass_enum_t iclass,
    xed_uint_t effective_operand_width,
    xed_uint_t number_of_operands,
    const xed_encoder_operand_t* operand_array) {

    xed_uint_t i;
    inst->mode=mode;
    inst->iclass = iclass;
    inst->effective_operand_width = effective_operand_width;
    inst->effective_address_width = 0;
    inst->prefixes.i = 0;
    xed_assert(number_of_operands < XED_ENCODER_OPERANDS_MAX);
    for(i=0;i<number_of_operands;i++) {
        inst->operands[i] = operand_array[i];
    }
    inst->noperands = number_of_operands;
}
