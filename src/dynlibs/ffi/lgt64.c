static lgt_noderef_t lgt_new_node_p(lgt_state_t st, lgt_code_t c, lgt_ptr_t u) {
   return _jit_new_node_p(st, c, u);
}

static lgt_noderef_t lgt_new_node_pw(lgt_state_t st, lgt_code_t c, lgt_ptr_t u, lgt_word_t v) {
   return _jit_new_node_pw(st, c, u, v);
}

static lgt_noderef_t lgt_new_node_pwd(lgt_state_t st, lgt_code_t c, lgt_ptr_t u, lgt_word_t v, lgt_double_t w) {
   return _jit_new_node_pwd(st, c, u, v, w);
}

static lgt_noderef_t lgt_new_node_pwf(lgt_state_t st, lgt_code_t c, lgt_ptr_t u, lgt_word_t v, lgt_float_t w) {
   return _jit_new_node_pwf(st, c, u, v, w);
}

static lgt_noderef_t lgt_new_node_pww(lgt_state_t st, lgt_code_t c, lgt_ptr_t u, lgt_word_t v, lgt_word_t w) {
   return _jit_new_node_pww(st, c, u, v, w);
}

static lgt_noderef_t lgt_new_node_qww(lgt_state_t st, lgt_code_t c, lgt_word_t l, lgt_word_t h, lgt_word_t v, lgt_word_t w) {
   return _jit_new_node_qww(st, c, l, h, v, w);
}

static lgt_noderef_t lgt_new_node_w(lgt_state_t st, lgt_code_t c, lgt_word_t u) {
   return _jit_new_node_w(st, c, u);
}

static lgt_noderef_t lgt_new_node_wd(lgt_state_t st, lgt_code_t c, lgt_word_t u, lgt_double_t v) {
   return _jit_new_node_wd(st, c, u, v);
}

static lgt_noderef_t lgt_new_node_wf(lgt_state_t st, lgt_code_t c, lgt_word_t u, lgt_float_t v) {
   return _jit_new_node_wf(st, c, u, v);
}

static lgt_noderef_t lgt_new_node_wp(lgt_state_t st, lgt_code_t c, lgt_word_t u, lgt_ptr_t v) {
   return _jit_new_node_wp(st, c, u, v);
}

static lgt_noderef_t lgt_new_node_ww(lgt_state_t st, lgt_code_t c, lgt_word_t u, lgt_word_t v) {
   return _jit_new_node_ww(st, c, u, v);
}

static lgt_noderef_t lgt_new_node_wwd(lgt_state_t st, lgt_code_t c, lgt_word_t u, lgt_word_t v, lgt_double_t w) {
   return _jit_new_node_wwd(st, c, u, v, w);
}

static lgt_noderef_t lgt_new_node_wwf(lgt_state_t st, lgt_code_t c, lgt_word_t u, lgt_word_t v, lgt_float_t w) {
   return _jit_new_node_wwf(st, c, u, v, w);
}

static lgt_noderef_t lgt_new_node_www(lgt_state_t st, lgt_code_t c, lgt_word_t u, lgt_word_t v, lgt_word_t w) {
   return _jit_new_node_www(st, c, u, v, w);
}

EXTERNML value jit_sml_address(value argsvec) {
   lgt_state_t s;
   lgt_noderef_t node;
   value result;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   node = lgt_noderef_val(Field(argsvec, 1));

   result = Val_lgt_ptr(_jit_address(s, node));

   return result;
}

EXTERNML value jit_sml_allocai(value argsvec) {
   lgt_state_t s;
   lgt_word_t u;
   value result;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_word_val(Field(argsvec, 1));

   result = Val_lgt_int(_jit_allocai(s, u));

   return result;
}

EXTERNML value jit_sml_arg(value argsvec) {
   lgt_state_t s;
   value result;

   s = lgt_state_val(argsvec);

   result = Val_lgt_noderef(_jit_arg(s));

   return result;
}

EXTERNML value jit_sml_arg_d(value argsvec) {
   lgt_state_t s;
   value result;

   s = lgt_state_val(argsvec);

   result = Val_lgt_noderef(_jit_arg_d(s));

   return result;
}

EXTERNML value jit_sml_arg_f(value argsvec) {
   lgt_state_t s;
   value result;

   s = lgt_state_val(argsvec);

   result = Val_lgt_noderef(_jit_arg_f(s));

   return result;
}

EXTERNML value jit_sml_clear_state(value argsvec) {
   lgt_state_t s;

   s = lgt_state_val(argsvec);

   _jit_clear_state(s);

   return Val_unit;
}

EXTERNML value jit_sml_destroy_state(value argsvec) {
   lgt_state_t s;

   s = lgt_state_val(argsvec);

   _jit_destroy_state(s);

   return Val_unit;
}

EXTERNML value jit_sml_disassemble(value argsvec) {
   lgt_state_t s;

   s = lgt_state_val(argsvec);

   _jit_disassemble(s);

   return Val_unit;
}

EXTERNML value jit_sml_ellipsis(value argsvec) {
   lgt_state_t s;

   s = lgt_state_val(argsvec);

   _jit_ellipsis(s);

   return Val_unit;
}

EXTERNML value jit_sml_emit(value argsvec) {
   lgt_state_t s;
   value result;

   s = lgt_state_val(argsvec);

   result = Val_lgt_ptr(_jit_emit(s));

   return result;
}

EXTERNML value jit_sml_epilog(value argsvec) {
   lgt_state_t s;

   s = lgt_state_val(argsvec);

   _jit_epilog(s);

   return Val_unit;
}

EXTERNML value jit_sml_finishi(value argsvec) {
   lgt_state_t s;
   lgt_ptr_t u;
   value result;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_ptr_val(Field(argsvec, 1));

   result = Val_lgt_noderef(_jit_finishi(s, u));

   return result;
}

EXTERNML value jit_sml_finishr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));

   _jit_finishr(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_forward(value argsvec) {
   lgt_state_t s;

   s = lgt_state_val(argsvec);

   _jit_forward(s);

   return Val_unit;
}

EXTERNML value jit_sml_getarg_c(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_noderef_t v;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_noderef_val(Field(argsvec, 2));

   _jit_getarg_c(s, u, v);

   return Val_unit;
}

EXTERNML value jit_sml_getarg_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_noderef_t v;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_noderef_val(Field(argsvec, 2));

   _jit_getarg_d(s, u, v);

   return Val_unit;
}

EXTERNML value jit_sml_getarg_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_noderef_t v;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_noderef_val(Field(argsvec, 2));

   _jit_getarg_f(s, u, v);

   return Val_unit;
}

EXTERNML value jit_sml_getarg_i(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_noderef_t v;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_noderef_val(Field(argsvec, 2));

   _jit_getarg_i(s, u, v);

   return Val_unit;
}

EXTERNML value jit_sml_getarg_l(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_noderef_t v;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_noderef_val(Field(argsvec, 2));

   _jit_getarg_l(s, u, v);

   return Val_unit;
}

EXTERNML value jit_sml_getarg(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_noderef_t v;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_noderef_val(Field(argsvec, 2));

   _jit_getarg_l(s, u, v);

   return Val_unit;
}

EXTERNML value jit_sml_getarg_s(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_noderef_t v;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_noderef_val(Field(argsvec, 2));

   _jit_getarg_s(s, u, v);

   return Val_unit;
}

EXTERNML value jit_sml_getarg_uc(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_noderef_t v;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_noderef_val(Field(argsvec, 2));

   _jit_getarg_uc(s, u, v);

   return Val_unit;
}

EXTERNML value jit_sml_getarg_ui(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_noderef_t v;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_noderef_val(Field(argsvec, 2));

   _jit_getarg_ui(s, u, v);

   return Val_unit;
}

EXTERNML value jit_sml_getarg_us(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_noderef_t v;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_noderef_val(Field(argsvec, 2));

   _jit_getarg_us(s, u, v);

   return Val_unit;
}

EXTERNML value jit_sml_indirect(value argsvec) {
   lgt_state_t s;
   value result;

   s = lgt_state_val(argsvec);

   result = Val_lgt_noderef(_jit_indirect(s));

   return result;
}

EXTERNML value jit_sml_label(value argsvec) {
   lgt_state_t s;

   s = lgt_state_val(argsvec);

   _jit_label(s);

   return Val_unit;
}

EXTERNML value jit_sml_new_node(value argsvec) {
   lgt_state_t s;
   lgt_code_t c;
   value result;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   c = lgt_code_val(Field(argsvec, 1));

   result = Val_lgt_noderef(_jit_new_node(s, c));

   return result;
}

EXTERNML value jit_sml_note(value argsvec) {
   lgt_state_t s;
   lgt_ptr_t u;
   lgt_word_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_ptr_val(Field(argsvec, 1));
   v = lgt_word_val(Field(argsvec, 2));

   result = Val_lgt_noderef(_jit_note(s, u, v));

   return result;
}

EXTERNML value jit_sml_patch(value argsvec) {
   lgt_state_t s;
   lgt_noderef_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_noderef_val(Field(argsvec, 1));

   _jit_patch(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_patch_abs(value argsvec) {
   lgt_state_t s;
   lgt_noderef_t u;
   lgt_ptr_t v;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_noderef_val(Field(argsvec, 1));
   v = lgt_ptr_val(Field(argsvec, 2));

   _jit_patch_abs(s, u, v);

   return Val_unit;
}

EXTERNML value jit_sml_patch_at(value argsvec) {
   lgt_state_t s;
   lgt_noderef_t u;
   lgt_noderef_t v;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_noderef_val(Field(argsvec, 1));
   v = lgt_noderef_val(Field(argsvec, 2));

   _jit_patch_at(s, u, v);

   return Val_unit;
}

EXTERNML value jit_sml_prepare(value argsvec) {
   lgt_state_t s;

   s = lgt_state_val(argsvec);

   _jit_prepare(s);

   return Val_unit;
}

EXTERNML value jit_sml_print(value argsvec) {
   lgt_state_t s;

   s = lgt_state_val(argsvec);

   _jit_print(s);

   return Val_unit;
}

EXTERNML value jit_sml_prolog(value argsvec) {
   lgt_state_t s;

   s = lgt_state_val(argsvec);

   _jit_prolog(s);

   return Val_unit;
}

EXTERNML value jit_sml_pushargi(value argsvec) {
   lgt_state_t s;
   lgt_word_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_word_val(Field(argsvec, 1));

   _jit_pushargi(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_pushargi_d(value argsvec) {
   lgt_state_t s;
   lgt_double_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_double_val(Field(argsvec, 1));

   _jit_pushargi_d(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_pushargi_f(value argsvec) {
   lgt_state_t s;
   lgt_float_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_float_val(Field(argsvec, 1));

   _jit_pushargi_f(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_pushargr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));

   _jit_pushargr(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_pushargr_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));

   _jit_pushargr_d(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_pushargr_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));

   _jit_pushargr_f(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_realize(value argsvec) {
   lgt_state_t s;

   s = lgt_state_val(argsvec);

   _jit_realize(s);

   return Val_unit;
}

EXTERNML value jit_sml_ret(value argsvec) {
   lgt_state_t s;

   s = lgt_state_val(argsvec);

   _jit_ret(s);

   return Val_unit;
}

EXTERNML value jit_sml_reti(value argsvec) {
   lgt_state_t s;
   lgt_word_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_word_val(Field(argsvec, 1));

   _jit_reti(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_reti_d(value argsvec) {
   lgt_state_t s;
   lgt_double_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_double_val(Field(argsvec, 1));

   _jit_reti_d(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_reti_f(value argsvec) {
   lgt_state_t s;
   lgt_float_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_float_val(Field(argsvec, 1));

   _jit_reti_f(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_retr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));

   _jit_retr(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_retr_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));

   _jit_retr_d(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_retr_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));

   _jit_retr_f(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_retval_c(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));

   _jit_retval_c(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_retval_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));

   _jit_retval_d(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_retval_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));

   _jit_retval_f(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_retval_i(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));

   _jit_retval_i(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_retval_l(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));

   _jit_retval_l(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_retval(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));

   _jit_retval_l(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_retval_s(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));

   _jit_retval_s(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_retval_uc(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));

   _jit_retval_uc(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_retval_ui(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));

   _jit_retval_ui(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_retval_us(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));

   _jit_retval_us(s, u);

   return Val_unit;
}

EXTERNML value jit_sml_absr_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_absr_d, u, v));

   return result;
}

EXTERNML value jit_sml_absr_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_absr_f, u, v));

   return result;
}

EXTERNML value jit_sml_addci(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_addci, u, v, w));

   return result;
}

EXTERNML value jit_sml_addcr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_addcr, u, v, w));

   return result;
}

EXTERNML value jit_sml_addi(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_addi, u, v, w));

   return result;
}

EXTERNML value jit_sml_addi_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_double_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwd(s, jit_code_addi_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_addi_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_float_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwf(s, jit_code_addi_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_addr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_addr, u, v, w));

   return result;
}

EXTERNML value jit_sml_addr_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_addr_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_addr_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_addr_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_addxi(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_addxi, u, v, w));

   return result;
}

EXTERNML value jit_sml_addxr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_addxr, u, v, w));

   return result;
}

EXTERNML value jit_sml_andi(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_andi, u, v, w));

   return result;
}

EXTERNML value jit_sml_andr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_andr, u, v, w));

   return result;
}

EXTERNML value jit_sml_beqi(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_word_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_beqi, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_beqi_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_double_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwd(s, jit_code_beqi_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_beqi_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_float_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwf(s, jit_code_beqi_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_beqr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_beqr, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_beqr_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_beqr_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_beqr_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_beqr_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bgei(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_word_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bgei, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bgei_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_double_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwd(s, jit_code_bgei_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bgei_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_float_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwf(s, jit_code_bgei_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bgei_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_word_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bgei_u, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bger(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bger, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bger_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bger_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bger_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bger_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bger_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bger_u, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bgti(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_word_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bgti, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bgti_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_double_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwd(s, jit_code_bgti_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bgti_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_float_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwf(s, jit_code_bgti_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bgti_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_word_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bgti_u, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bgtr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bgtr, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bgtr_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bgtr_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bgtr_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bgtr_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bgtr_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bgtr_u, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_blei(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_word_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_blei, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_blei_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_double_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwd(s, jit_code_blei_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_blei_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_float_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwf(s, jit_code_blei_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_blei_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_word_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_blei_u, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bler(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bler, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bler_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bler_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bler_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bler_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bler_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bler_u, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bltgti_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_double_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwd(s, jit_code_bltgti_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bltgti_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_float_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwf(s, jit_code_bltgti_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bltgtr_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bltgtr_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bltgtr_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bltgtr_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_blti(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_word_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_blti, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_blti_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_double_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwd(s, jit_code_blti_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_blti_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_float_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwf(s, jit_code_blti_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_blti_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_word_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_blti_u, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bltr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bltr, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bltr_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bltr_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bltr_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bltr_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bltr_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bltr_u, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bmci(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_word_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bmci, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bmcr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bmcr, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bmsi(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_word_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bmsi, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bmsr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bmsr, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bnei(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_word_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bnei, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bnei_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_double_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwd(s, jit_code_bnei_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bnei_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_float_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwf(s, jit_code_bnei_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bner(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bner, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bner_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bner_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bner_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bner_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_boaddi(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_word_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_boaddi, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_boaddi_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_word_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_boaddi_u, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_boaddr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_boaddr, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_boaddr_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_boaddr_u, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bordi_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_double_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwd(s, jit_code_bordi_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bordi_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_float_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwf(s, jit_code_bordi_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bordr_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bordr_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bordr_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bordr_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bosubi(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_word_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bosubi, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bosubi_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_word_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bosubi_u, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bosubr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bosubr, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bosubr_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bosubr_u, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_buneqi_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_double_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwd(s, jit_code_buneqi_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_buneqi_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_float_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwf(s, jit_code_buneqi_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_buneqr_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_buneqr_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_buneqr_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_buneqr_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bungei_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_double_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwd(s, jit_code_bungei_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bungei_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_float_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwf(s, jit_code_bungei_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bunger_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bunger_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bunger_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bunger_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bungti_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_double_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwd(s, jit_code_bungti_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bungti_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_float_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwf(s, jit_code_bungti_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bungtr_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bungtr_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bungtr_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bungtr_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bunlei_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_double_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwd(s, jit_code_bunlei_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bunlei_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_float_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwf(s, jit_code_bunlei_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bunler_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bunler_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bunler_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bunler_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bunlti_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_double_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwd(s, jit_code_bunlti_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bunlti_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_float_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwf(s, jit_code_bunlti_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bunltr_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bunltr_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bunltr_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bunltr_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bunordi_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_double_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwd(s, jit_code_bunordi_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bunordi_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_float_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pwf(s, jit_code_bunordi_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bunordr_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bunordr_d, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bunordr_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_fpr_val(Field(argsvec, 1));
   w = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bunordr_f, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bxaddi(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_word_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bxaddi, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bxaddi_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_word_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bxaddi_u, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bxaddr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bxaddr, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bxaddr_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bxaddr_u, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bxsubi(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_word_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bxsubi, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bxsubi_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_word_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bxsubi_u, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bxsubr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bxsubr, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_bxsubr_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   v = lgt_gpr_val(Field(argsvec, 1));
   w = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pww(s, jit_code_bxsubr_u, NULL, v, w));

   return result;
}

EXTERNML value jit_sml_calli(value argsvec) {
   lgt_state_t s;
   lgt_ptr_t u;
   value result;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_ptr_val(Field(argsvec, 1));

   result = Val_lgt_noderef(lgt_new_node_p(s, jit_code_calli, u));

   return result;
}

EXTERNML value jit_sml_callr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   value result;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));

   result = Val_lgt_noderef(lgt_new_node_w(s, jit_code_callr, u));

   return result;
}

EXTERNML value jit_sml_comr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_comr, u, v));

   return result;
}

EXTERNML value jit_sml_divi(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_divi, u, v, w));

   return result;
}

EXTERNML value jit_sml_divi_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_double_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwd(s, jit_code_divi_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_divi_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_float_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwf(s, jit_code_divi_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_divi_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_divi_u, u, v, w));

   return result;
}

EXTERNML value jit_sml_divr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_divr, u, v, w));

   return result;
}

EXTERNML value jit_sml_divr_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_divr_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_divr_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_divr_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_divr_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_divr_u, u, v, w));

   return result;
}

EXTERNML value jit_sml_eqi(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_eqi, u, v, w));

   return result;
}

EXTERNML value jit_sml_eqi_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_double_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwd(s, jit_code_eqi_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_eqi_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_float_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwf(s, jit_code_eqi_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_eqr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_eqr, u, v, w));

   return result;
}

EXTERNML value jit_sml_eqr_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_eqr_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_eqr_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_eqr_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_extr_c(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_extr_c, u, v));

   return result;
}

EXTERNML value jit_sml_extr_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_extr_d, u, v));

   return result;
}

EXTERNML value jit_sml_extr_d_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_extr_d_f, u, v));

   return result;
}

EXTERNML value jit_sml_extr_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_extr_f, u, v));

   return result;
}

EXTERNML value jit_sml_extr_f_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_extr_f_d, u, v));

   return result;
}

EXTERNML value jit_sml_extr_i(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_extr_i, u, v));

   return result;
}

EXTERNML value jit_sml_extr_s(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_extr_s, u, v));

   return result;
}

EXTERNML value jit_sml_extr_uc(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_extr_uc, u, v));

   return result;
}

EXTERNML value jit_sml_extr_ui(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_extr_ui, u, v));

   return result;
}

EXTERNML value jit_sml_extr_us(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_extr_us, u, v));

   return result;
}

EXTERNML value jit_sml_gei(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_gei, u, v, w));

   return result;
}

EXTERNML value jit_sml_gei_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_double_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwd(s, jit_code_gei_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_gei_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_float_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwf(s, jit_code_gei_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_gei_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_gei_u, u, v, w));

   return result;
}

EXTERNML value jit_sml_ger(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ger, u, v, w));

   return result;
}

EXTERNML value jit_sml_ger_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ger_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_ger_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ger_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_ger_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ger_u, u, v, w));

   return result;
}

EXTERNML value jit_sml_gti(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_gti, u, v, w));

   return result;
}

EXTERNML value jit_sml_gti_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_double_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwd(s, jit_code_gti_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_gti_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_float_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwf(s, jit_code_gti_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_gti_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_gti_u, u, v, w));

   return result;
}

EXTERNML value jit_sml_gtr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_gtr, u, v, w));

   return result;
}

EXTERNML value jit_sml_gtr_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_gtr_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_gtr_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_gtr_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_gtr_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_gtr_u, u, v, w));

   return result;
}

EXTERNML value jit_sml_htonr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_htonr, u, v));

   return result;
}

EXTERNML value jit_sml_jmpi(value argsvec) {
   lgt_state_t s;
   value result;

   s = lgt_state_val(argsvec);

   result = Val_lgt_noderef(lgt_new_node_p(s, jit_code_jmpi, NULL));

   return result;
}

EXTERNML value jit_sml_jmpr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   value result;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));

   result = Val_lgt_noderef(lgt_new_node_w(s, jit_code_jmpr, u));

   return result;
}

EXTERNML value jit_sml_ldi_c(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_ptr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_ptr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_wp(s, jit_code_ldi_c, u, v));

   return result;
}

EXTERNML value jit_sml_ldi_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_ptr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_ptr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_wp(s, jit_code_ldi_d, u, v));

   return result;
}

EXTERNML value jit_sml_ldi_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_ptr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_ptr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_wp(s, jit_code_ldi_f, u, v));

   return result;
}

EXTERNML value jit_sml_ldi_i(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_ptr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_ptr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_wp(s, jit_code_ldi_i, u, v));

   return result;
}

EXTERNML value jit_sml_ldi_l(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_ptr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_ptr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_wp(s, jit_code_ldi_l, u, v));

   return result;
}

EXTERNML value jit_sml_ldi(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_ptr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_ptr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_wp(s, jit_code_ldi_l, u, v));

   return result;
}

EXTERNML value jit_sml_ldi_s(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_ptr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_ptr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_wp(s, jit_code_ldi_s, u, v));

   return result;
}

EXTERNML value jit_sml_ldi_uc(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_ptr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_ptr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_wp(s, jit_code_ldi_uc, u, v));

   return result;
}

EXTERNML value jit_sml_ldi_ui(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_ptr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_ptr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_wp(s, jit_code_ldi_ui, u, v));

   return result;
}

EXTERNML value jit_sml_ldi_us(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_ptr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_ptr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_wp(s, jit_code_ldi_us, u, v));

   return result;
}

EXTERNML value jit_sml_ldr_c(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_ldr_c, u, v));

   return result;
}

EXTERNML value jit_sml_ldr_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_ldr_d, u, v));

   return result;
}

EXTERNML value jit_sml_ldr_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_ldr_f, u, v));

   return result;
}

EXTERNML value jit_sml_ldr_i(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_ldr_i, u, v));

   return result;
}

EXTERNML value jit_sml_ldr_l(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_ldr_l, u, v));

   return result;
}

EXTERNML value jit_sml_ldr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_ldr_l, u, v));

   return result;
}

EXTERNML value jit_sml_ldr_s(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_ldr_s, u, v));

   return result;
}

EXTERNML value jit_sml_ldr_uc(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_ldr_uc, u, v));

   return result;
}

EXTERNML value jit_sml_ldr_ui(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_ldr_ui, u, v));

   return result;
}

EXTERNML value jit_sml_ldr_us(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_ldr_us, u, v));

   return result;
}

EXTERNML value jit_sml_ldxi_c(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ldxi_c, u, v, w));

   return result;
}

EXTERNML value jit_sml_ldxi_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ldxi_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_ldxi_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ldxi_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_ldxi_i(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ldxi_i, u, v, w));

   return result;
}

EXTERNML value jit_sml_ldxi_l(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ldxi_l, u, v, w));

   return result;
}

EXTERNML value jit_sml_ldxi(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ldxi_l, u, v, w));

   return result;
}

EXTERNML value jit_sml_ldxi_s(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ldxi_s, u, v, w));

   return result;
}

EXTERNML value jit_sml_ldxi_uc(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ldxi_uc, u, v, w));

   return result;
}

EXTERNML value jit_sml_ldxi_ui(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ldxi_ui, u, v, w));

   return result;
}

EXTERNML value jit_sml_ldxi_us(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ldxi_us, u, v, w));

   return result;
}

EXTERNML value jit_sml_ldxr_c(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ldxr_c, u, v, w));

   return result;
}

EXTERNML value jit_sml_ldxr_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ldxr_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_ldxr_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ldxr_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_ldxr_i(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ldxr_i, u, v, w));

   return result;
}

EXTERNML value jit_sml_ldxr_l(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ldxr_l, u, v, w));

   return result;
}

EXTERNML value jit_sml_ldxr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ldxr_l, u, v, w));

   return result;
}

EXTERNML value jit_sml_ldxr_s(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ldxr_s, u, v, w));

   return result;
}

EXTERNML value jit_sml_ldxr_uc(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ldxr_uc, u, v, w));

   return result;
}

EXTERNML value jit_sml_ldxr_ui(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ldxr_ui, u, v, w));

   return result;
}

EXTERNML value jit_sml_ldxr_us(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ldxr_us, u, v, w));

   return result;
}

EXTERNML value jit_sml_lei(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_lei, u, v, w));

   return result;
}

EXTERNML value jit_sml_lei_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_double_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwd(s, jit_code_lei_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_lei_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_float_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwf(s, jit_code_lei_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_lei_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_lei_u, u, v, w));

   return result;
}

EXTERNML value jit_sml_ler(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ler, u, v, w));

   return result;
}

EXTERNML value jit_sml_ler_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ler_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_ler_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ler_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_ler_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ler_u, u, v, w));

   return result;
}

EXTERNML value jit_sml_live(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   value result;

   lgt_chk_args(argsvec, 2);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));

   result = Val_lgt_noderef(lgt_new_node_w(s, jit_code_live, u));

   return result;
}

EXTERNML value jit_sml_lshi(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_lshi, u, v, w));

   return result;
}

EXTERNML value jit_sml_lshr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_lshr, u, v, w));

   return result;
}

EXTERNML value jit_sml_ltgti_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_double_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwd(s, jit_code_ltgti_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_ltgti_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_float_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwf(s, jit_code_ltgti_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_ltgtr_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ltgtr_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_ltgtr_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ltgtr_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_lti(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_lti, u, v, w));

   return result;
}

EXTERNML value jit_sml_lti_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_double_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwd(s, jit_code_lti_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_lti_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_float_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwf(s, jit_code_lti_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_lti_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_lti_u, u, v, w));

   return result;
}

EXTERNML value jit_sml_ltr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ltr, u, v, w));

   return result;
}

EXTERNML value jit_sml_ltr_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ltr_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_ltr_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ltr_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_ltr_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ltr_u, u, v, w));

   return result;
}

EXTERNML value jit_sml_movi(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_word_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_word_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_movi, u, v));

   return result;
}

EXTERNML value jit_sml_movi_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_double_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_double_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_wd(s, jit_code_movi_d, u, v));

   return result;
}

EXTERNML value jit_sml_movi_d_w(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_double_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_double_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_wd(s, jit_code_movi_d_w, u, v));

   return result;
}

EXTERNML value jit_sml_movi_d_ww(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_double_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwd(s, jit_code_movi_d_ww, u, v, w));

   return result;
}

EXTERNML value jit_sml_movi_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_float_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_float_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_wf(s, jit_code_movi_f, u, v));

   return result;
}

EXTERNML value jit_sml_movi_f_w(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_float_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_float_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_wf(s, jit_code_movi_f_w, u, v));

   return result;
}

EXTERNML value jit_sml_movr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_movr, u, v));

   return result;
}

EXTERNML value jit_sml_movr_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_movr_d, u, v));

   return result;
}

EXTERNML value jit_sml_movr_d_w(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_movr_d_w, u, v));

   return result;
}

EXTERNML value jit_sml_movr_d_ww(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_movr_d_ww, u, v, w));

   return result;
}

EXTERNML value jit_sml_movr_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_movr_f, u, v));

   return result;
}

EXTERNML value jit_sml_movr_f_w(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_movr_f_w, u, v));

   return result;
}

EXTERNML value jit_sml_movr_w_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_movr_w_d, u, v));

   return result;
}

EXTERNML value jit_sml_movr_w_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_movr_w_f, u, v));

   return result;
}

EXTERNML value jit_sml_movr_ww_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_movr_ww_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_muli(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_muli, u, v, w));

   return result;
}

EXTERNML value jit_sml_muli_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_double_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwd(s, jit_code_muli_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_muli_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_float_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwf(s, jit_code_muli_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_mulr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_mulr, u, v, w));

   return result;
}

EXTERNML value jit_sml_mulr_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_mulr_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_mulr_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_mulr_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_negr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_negr, u, v));

   return result;
}

EXTERNML value jit_sml_negr_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_negr_d, u, v));

   return result;
}

EXTERNML value jit_sml_negr_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_negr_f, u, v));

   return result;
}

EXTERNML value jit_sml_nei(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_nei, u, v, w));

   return result;
}

EXTERNML value jit_sml_nei_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_double_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwd(s, jit_code_nei_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_nei_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_float_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwf(s, jit_code_nei_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_ner(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ner, u, v, w));

   return result;
}

EXTERNML value jit_sml_ner_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ner_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_ner_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ner_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_ntohr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_htonr, u, v));

   return result;
}

EXTERNML value jit_sml_ordi_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_double_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwd(s, jit_code_ordi_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_ordi_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_float_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwf(s, jit_code_ordi_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_ordr_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ordr_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_ordr_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ordr_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_ori(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ori, u, v, w));

   return result;
}

EXTERNML value jit_sml_orr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_orr, u, v, w));

   return result;
}

EXTERNML value jit_sml_qdivi(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t l;
   lgt_gpr_t h;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 5);

   s = lgt_state_val(Field(argsvec, 0));
   l = lgt_gpr_val(Field(argsvec, 1));
   h = lgt_gpr_val(Field(argsvec, 2));
   v = lgt_gpr_val(Field(argsvec, 3));
   w = lgt_word_val(Field(argsvec, 4));

   result = Val_lgt_noderef(lgt_new_node_qww(s, jit_code_qdivi, l, h, v, w));

   return result;
}

EXTERNML value jit_sml_qdivi_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t l;
   lgt_gpr_t h;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 5);

   s = lgt_state_val(Field(argsvec, 0));
   l = lgt_gpr_val(Field(argsvec, 1));
   h = lgt_gpr_val(Field(argsvec, 2));
   v = lgt_gpr_val(Field(argsvec, 3));
   w = lgt_word_val(Field(argsvec, 4));

   result = Val_lgt_noderef(lgt_new_node_qww(s, jit_code_qdivi_u, l, h, v, w));

   return result;
}

EXTERNML value jit_sml_qdivr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t l;
   lgt_gpr_t h;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 5);

   s = lgt_state_val(Field(argsvec, 0));
   l = lgt_gpr_val(Field(argsvec, 1));
   h = lgt_gpr_val(Field(argsvec, 2));
   v = lgt_gpr_val(Field(argsvec, 3));
   w = lgt_gpr_val(Field(argsvec, 4));

   result = Val_lgt_noderef(lgt_new_node_qww(s, jit_code_qdivr, l, h, v, w));

   return result;
}

EXTERNML value jit_sml_qdivr_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t l;
   lgt_gpr_t h;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 5);

   s = lgt_state_val(Field(argsvec, 0));
   l = lgt_gpr_val(Field(argsvec, 1));
   h = lgt_gpr_val(Field(argsvec, 2));
   v = lgt_gpr_val(Field(argsvec, 3));
   w = lgt_gpr_val(Field(argsvec, 4));

   result = Val_lgt_noderef(lgt_new_node_qww(s, jit_code_qdivr_u, l, h, v, w));

   return result;
}

EXTERNML value jit_sml_qmuli(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t l;
   lgt_gpr_t h;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 5);

   s = lgt_state_val(Field(argsvec, 0));
   l = lgt_gpr_val(Field(argsvec, 1));
   h = lgt_gpr_val(Field(argsvec, 2));
   v = lgt_gpr_val(Field(argsvec, 3));
   w = lgt_word_val(Field(argsvec, 4));

   result = Val_lgt_noderef(lgt_new_node_qww(s, jit_code_qmuli, l, h, v, w));

   return result;
}

EXTERNML value jit_sml_qmuli_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t l;
   lgt_gpr_t h;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 5);

   s = lgt_state_val(Field(argsvec, 0));
   l = lgt_gpr_val(Field(argsvec, 1));
   h = lgt_gpr_val(Field(argsvec, 2));
   v = lgt_gpr_val(Field(argsvec, 3));
   w = lgt_word_val(Field(argsvec, 4));

   result = Val_lgt_noderef(lgt_new_node_qww(s, jit_code_qmuli_u, l, h, v, w));

   return result;
}

EXTERNML value jit_sml_qmulr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t l;
   lgt_gpr_t h;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 5);

   s = lgt_state_val(Field(argsvec, 0));
   l = lgt_gpr_val(Field(argsvec, 1));
   h = lgt_gpr_val(Field(argsvec, 2));
   v = lgt_gpr_val(Field(argsvec, 3));
   w = lgt_gpr_val(Field(argsvec, 4));

   result = Val_lgt_noderef(lgt_new_node_qww(s, jit_code_qmulr, l, h, v, w));

   return result;
}

EXTERNML value jit_sml_qmulr_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t l;
   lgt_gpr_t h;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 5);

   s = lgt_state_val(Field(argsvec, 0));
   l = lgt_gpr_val(Field(argsvec, 1));
   h = lgt_gpr_val(Field(argsvec, 2));
   v = lgt_gpr_val(Field(argsvec, 3));
   w = lgt_gpr_val(Field(argsvec, 4));

   result = Val_lgt_noderef(lgt_new_node_qww(s, jit_code_qmulr_u, l, h, v, w));

   return result;
}

EXTERNML value jit_sml_remi(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_remi, u, v, w));

   return result;
}

EXTERNML value jit_sml_remi_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_remi_u, u, v, w));

   return result;
}

EXTERNML value jit_sml_remr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_remr, u, v, w));

   return result;
}

EXTERNML value jit_sml_remr_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_remr_u, u, v, w));

   return result;
}

EXTERNML value jit_sml_rshi(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_rshi, u, v, w));

   return result;
}

EXTERNML value jit_sml_rshi_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_rshi_u, u, v, w));

   return result;
}

EXTERNML value jit_sml_rshr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_rshr, u, v, w));

   return result;
}

EXTERNML value jit_sml_rshr_u(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_rshr_u, u, v, w));

   return result;
}

EXTERNML value jit_sml_sqrtr_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_sqrtr_d, u, v));

   return result;
}

EXTERNML value jit_sml_sqrtr_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_sqrtr_f, u, v));

   return result;
}

EXTERNML value jit_sml_sti_c(value argsvec) {
   lgt_state_t s;
   lgt_ptr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_ptr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pw(s, jit_code_sti_c, u, v));

   return result;
}

EXTERNML value jit_sml_sti_d(value argsvec) {
   lgt_state_t s;
   lgt_ptr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_ptr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pw(s, jit_code_sti_d, u, v));

   return result;
}

EXTERNML value jit_sml_sti_f(value argsvec) {
   lgt_state_t s;
   lgt_ptr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_ptr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pw(s, jit_code_sti_f, u, v));

   return result;
}

EXTERNML value jit_sml_sti_i(value argsvec) {
   lgt_state_t s;
   lgt_ptr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_ptr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pw(s, jit_code_sti_i, u, v));

   return result;
}

EXTERNML value jit_sml_sti_l(value argsvec) {
   lgt_state_t s;
   lgt_ptr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_ptr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pw(s, jit_code_sti_l, u, v));

   return result;
}

EXTERNML value jit_sml_sti(value argsvec) {
   lgt_state_t s;
   lgt_ptr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_ptr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pw(s, jit_code_sti_l, u, v));

   return result;
}

EXTERNML value jit_sml_sti_s(value argsvec) {
   lgt_state_t s;
   lgt_ptr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_ptr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_pw(s, jit_code_sti_s, u, v));

   return result;
}

EXTERNML value jit_sml_str_c(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_str_c, u, v));

   return result;
}

EXTERNML value jit_sml_str_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_str_d, u, v));

   return result;
}

EXTERNML value jit_sml_str_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_str_f, u, v));

   return result;
}

EXTERNML value jit_sml_str_i(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_str_i, u, v));

   return result;
}

EXTERNML value jit_sml_str_l(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_str_l, u, v));

   return result;
}

EXTERNML value jit_sml_str(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_str_l, u, v));

   return result;
}

EXTERNML value jit_sml_str_s(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_str_s, u, v));

   return result;
}

EXTERNML value jit_sml_stxi_c(value argsvec) {
   lgt_state_t s;
   lgt_word_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_word_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_stxi_c, u, v, w));

   return result;
}

EXTERNML value jit_sml_stxi_d(value argsvec) {
   lgt_state_t s;
   lgt_word_t u;
   lgt_gpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_word_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_stxi_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_stxi_f(value argsvec) {
   lgt_state_t s;
   lgt_word_t u;
   lgt_gpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_word_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_stxi_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_stxi_i(value argsvec) {
   lgt_state_t s;
   lgt_word_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_word_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_stxi_i, u, v, w));

   return result;
}

EXTERNML value jit_sml_stxi_l(value argsvec) {
   lgt_state_t s;
   lgt_word_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_word_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_stxi_l, u, v, w));

   return result;
}

EXTERNML value jit_sml_stxi(value argsvec) {
   lgt_state_t s;
   lgt_word_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_word_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_stxi_l, u, v, w));

   return result;
}

EXTERNML value jit_sml_stxi_s(value argsvec) {
   lgt_state_t s;
   lgt_word_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_word_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_stxi_s, u, v, w));

   return result;
}

EXTERNML value jit_sml_stxr_c(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_stxr_c, u, v, w));

   return result;
}

EXTERNML value jit_sml_stxr_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_stxr_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_stxr_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_stxr_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_stxr_i(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_stxr_i, u, v, w));

   return result;
}

EXTERNML value jit_sml_stxr_l(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_stxr_l, u, v, w));

   return result;
}

EXTERNML value jit_sml_stxr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_stxr_l, u, v, w));

   return result;
}

EXTERNML value jit_sml_stxr_s(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_stxr_s, u, v, w));

   return result;
}

EXTERNML value jit_sml_subci(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_subci, u, v, w));

   return result;
}

EXTERNML value jit_sml_subcr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_subcr, u, v, w));

   return result;
}

EXTERNML value jit_sml_subi(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_subi, u, v, w));

   return result;
}

EXTERNML value jit_sml_subi_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_double_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwd(s, jit_code_subi_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_subi_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_float_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwf(s, jit_code_subi_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_subr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_subr, u, v, w));

   return result;
}

EXTERNML value jit_sml_subr_d(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_subr_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_subr_f(value argsvec) {
   lgt_state_t s;
   lgt_fpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_fpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_subr_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_subxi(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_subxi, u, v, w));

   return result;
}

EXTERNML value jit_sml_subxr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_subxr, u, v, w));

   return result;
}

EXTERNML value jit_sml_truncr_d_i(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_truncr_d_i, u, v));

   return result;
}

EXTERNML value jit_sml_truncr_d_l(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_truncr_d_l, u, v));

   return result;
}

EXTERNML value jit_sml_truncr_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_truncr_d_l, u, v));

   return result;
}

EXTERNML value jit_sml_truncr_f_i(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_truncr_f_i, u, v));

   return result;
}

EXTERNML value jit_sml_truncr_f_l(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_truncr_f_l, u, v));

   return result;
}

EXTERNML value jit_sml_truncr_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   value result;

   lgt_chk_args(argsvec, 3);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));

   result = Val_lgt_noderef(lgt_new_node_ww(s, jit_code_truncr_f_l, u, v));

   return result;
}

EXTERNML value jit_sml_uneqi_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_double_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwd(s, jit_code_uneqi_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_uneqi_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_float_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwf(s, jit_code_uneqi_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_uneqr_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_uneqr_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_uneqr_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_uneqr_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_ungei_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_double_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwd(s, jit_code_ungei_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_ungei_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_float_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwf(s, jit_code_ungei_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_unger_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_unger_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_unger_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_unger_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_ungti_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_double_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwd(s, jit_code_ungti_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_ungti_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_float_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwf(s, jit_code_ungti_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_ungtr_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ungtr_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_ungtr_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_ungtr_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_unlei_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_double_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwd(s, jit_code_unlei_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_unlei_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_float_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwf(s, jit_code_unlei_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_unler_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_unler_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_unler_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_unler_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_unlti_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_double_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwd(s, jit_code_unlti_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_unlti_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_float_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwf(s, jit_code_unlti_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_unltr_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_unltr_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_unltr_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_unltr_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_unordi_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_double_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_double_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwd(s, jit_code_unordi_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_unordi_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_float_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_float_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_wwf(s, jit_code_unordi_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_unordr_d(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_unordr_d, u, v, w));

   return result;
}

EXTERNML value jit_sml_unordr_f(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_fpr_t v;
   lgt_fpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_fpr_val(Field(argsvec, 2));
   w = lgt_fpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_unordr_f, u, v, w));

   return result;
}

EXTERNML value jit_sml_xori(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_word_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_word_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_xori, u, v, w));

   return result;
}

EXTERNML value jit_sml_xorr(value argsvec) {
   lgt_state_t s;
   lgt_gpr_t u;
   lgt_gpr_t v;
   lgt_gpr_t w;
   value result;

   lgt_chk_args(argsvec, 4);

   s = lgt_state_val(Field(argsvec, 0));
   u = lgt_gpr_val(Field(argsvec, 1));
   v = lgt_gpr_val(Field(argsvec, 2));
   w = lgt_gpr_val(Field(argsvec, 3));

   result = Val_lgt_noderef(lgt_new_node_www(s, jit_code_xorr, u, v, w));

   return result;
}

