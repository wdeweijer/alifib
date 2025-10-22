type span = Positions.span option
type 'a node = { span : span; value : 'a }

type bd_kind = In | Out

type program = program_desc node

and program_desc = { program_blocks : block list }

and block = block_desc node

and block_desc =
  | Type_block of { type_block_instructions : c_instr_type list }
  | Complex_block of {
      complex_block_complex : complex;
      complex_block_locals : c_instr_local list;
    }

and c_instr_type = c_instr_type_desc node

and c_instr_type_desc =
  | CI_type_generator of generator_type
  | CI_type_dnamer of dnamer
  | CI_type_mnamer of mnamer
  | CI_type_include of include_statement

and c_instr = c_instr_desc node

and c_instr_desc =
  | CI_generator of generator
  | CI_dnamer of dnamer
  | CI_mnamer of mnamer
  | CI_include of include_statement
  | CI_attach of attach_statement

and c_instr_local = c_instr_local_desc node

and c_instr_local_desc =
  | CI_local_dnamer of dnamer
  | CI_local_mnamer of mnamer
  | CI_local_assert of assert_statement

and generator_type = generator_type_desc node

and generator_type_desc = {
  generator_type_generator : generator;
  generator_type_definition : complex;
}

and generator = generator_desc node

and generator_desc = {
  generator_name : name;
  generator_boundaries : boundaries option;
}

and boundaries = boundaries_desc node

and boundaries_desc = {
  boundaries_source : diagram;
  boundaries_target : diagram;
}

and complex = complex_desc node

and complex_desc = {
  complex_address : address option;
  complex_body : c_instr list;
}

and address = address_desc node

and address_desc = { address_segments : name list }

and morphism = morphism_desc node

and morphism_desc = { morphism_components : mcomp list }

and mcomp = mcomp_desc node

and mcomp_desc =
  | M_component_term of mterm
  | M_component_name of name

and mterm = mterm_desc node

and mterm_desc = {
  mterm_ext : mext;
  mterm_target : complex;
}

and mext = mext_desc node

and mext_desc = {
  mext_root : mroot option;
  mext_block : mblock;
}

and mroot = mroot_desc node

and mroot_desc =
  | M_root_name of name
  | M_root_compose of {
      mroot_component : mcomp;
      mroot_morphism : morphism;
    }

and mblock = mblock_desc node

and mblock_desc = { mblock_instructions : m_instr list }

and m_instr = m_instr_desc node

and m_instr_desc =
  | M_instr_diagram of {
      minstr_address : address;
      minstr_diagram : diagram;
    }

and mdef = mdef_desc node

and mdef_desc =
  | Mdef_ext of mext
  | Mdef_morphism of morphism

and mnamer = mnamer_desc node

and mnamer_desc = {
  mnamer_name : name;
  mnamer_address : address;
  mnamer_definition : mdef;
}

and dnamer = dnamer_desc node

and dnamer_desc = {
  dnamer_name : name;
  dnamer_boundaries : boundaries option;
  dnamer_body : diagram;
}

and include_statement = include_statement_desc node

and include_statement_desc = {
  include_address : address;
  include_alias : name option;
}

and attach_statement = attach_statement_desc node

and attach_statement_desc = {
  attach_name : name;
  attach_address : address;
  attach_along : mdef option;
}

and assert_statement = assert_statement_desc node

and assert_statement_desc = {
  assert_left : diagram;
  assert_right : diagram;
}

and diagram = diagram_desc node

and diagram_desc = {
  diagram_first : d_concat;
  diagram_rest : diagram_suffix list;
}

and diagram_suffix = diagram_suffix_desc node

and diagram_suffix_desc = {
  diagram_suffix_nat : nat;
  diagram_suffix_concat : d_concat;
}

and d_concat = d_concat_desc node

and d_concat_desc = { d_concat_exprs : d_expr list }

and d_expr = d_expr_desc node

and d_expr_desc = { d_expr_components : d_comp list }

and d_comp = d_comp_desc node

and d_comp_desc =
  | D_comp_mterm of mterm
  | D_comp_dterm of d_term
  | D_comp_name of name
  | D_comp_bd of bd
  | D_comp_hole

and d_term = d_term_desc node

and d_term_desc =
  | D_term_indexed of {
      dterm_diagram : diagram;
      dterm_index : nat;
      dterm_tail : d_concat;
    }
  | D_term_pair of {
      dterm_concat : d_concat;
      dterm_expr : d_expr;
    }

and name = Id.Local.t node

and nat = int node

and bd = bd_kind node

type complex_named = complex

let node ?span value = { span; value }

let empty = node { program_blocks = [] }

let of_tokens _ = empty
