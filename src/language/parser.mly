%{
open Ast

module Pos = Positions
module Tk = Token
module Local = Id.Local

let mk ?span value = Ast.node ?span value

let token_span token = Some (Tk.span token)

let merge_span a b =
  match (a, b) with
  | Some a, Some b -> Some (Pos.merge a b)
  | Some a, None -> Some a
  | None, Some b -> Some b
  | None, None -> None

let merge_spans spans = List.fold_left merge_span None spans

let node_span node = node.span

let option_span = function
  | Some node -> node.span
  | None -> None

let list_span nodes =
  merge_spans (List.map node_span nodes)

let make_name token =
  let span = token_span token in
  match Tk.kind token with
  | Tk.Identifier ident ->
      mk ?span (Local.make ident)
  | Tk.Keyword `Type ->
      mk ?span (Local.make "Type")
  | _ ->
      failwith "expected identifier"

let make_nat token =
  let span = token_span token in
  match Tk.kind token with
  | Tk.Nat digits ->
      let value = int_of_string digits in
      mk ?span value
  | _ ->
      failwith "expected natural number"

let make_bd token =
  let span = token_span token in
  match Tk.kind token with
  | Tk.Keyword `In ->
      mk ?span In
  | Tk.Keyword `Out ->
      mk ?span Out
  | _ ->
      failwith "expected boundary keyword"

let span_of_nodes nodes =
  match nodes with
  | [] ->
      None
  | _ ->
      list_span nodes

type diagram_acc = {
  acc_span : span;
  acc_first : d_concat;
  acc_rest_rev : diagram_suffix list;
}

let make_diagram_suffix paste nat concat =
  let span =
    merge_spans
      [ token_span paste
      ; node_span nat
      ; node_span concat
      ]
  in
  mk ?span
    { diagram_suffix_nat = nat
    ; diagram_suffix_concat = concat
    }

let diagram_acc_init first =
  { acc_span = first.span; acc_first = first; acc_rest_rev = [] }

let diagram_acc_extend acc paste nat concat =
  let suffix = make_diagram_suffix paste nat concat in
  let span = merge_spans [acc.acc_span; suffix.span] in
  { acc_span = span
  ; acc_first = acc.acc_first
  ; acc_rest_rev = suffix :: acc.acc_rest_rev
  }

let diagram_from_acc acc =
  let rest = List.rev acc.acc_rest_rev in
  mk ?span:acc.acc_span
    { diagram_first = acc.acc_first
    ; diagram_rest = rest
    }
%} 

%token <Token.t> AT
%token <Token.t> TYPE
%token <Token.t> INCLUDE
%token <Token.t> ATTACH
%token <Token.t> ALONG
%token <Token.t> ASSERT
%token <Token.t> IN
%token <Token.t> OUT
%token <Token.t> LET
%token <Token.t> AS
%token <Token.t> LBRACE
%token <Token.t> RBRACE
%token <Token.t> LBRACKET
%token <Token.t> RBRACKET
%token <Token.t> LPAREN
%token <Token.t> RPAREN
%token <Token.t> DOT
%token <Token.t> COMMA
%token <Token.t> PASTE
%token <Token.t> COLON
%token <Token.t> OF_SHAPE
%token <Token.t> MAPS_TO
%token <Token.t> ARROW
%token <Token.t> HAS_VALUE
%token <Token.t> EQUAL
%token <Token.t> HOLE
%token <Token.t> IDENT
%token <Token.t> NAT
%token EOF

%start <Ast.program> program

%%

program:
  | blocks_rev=block_list EOF {
      let blocks = List.rev blocks_rev in
      let span = span_of_nodes blocks in
      mk ?span { program_blocks = blocks }
    }

block_list:
  | { [] }
  | blocks_rev=block_list block=block { block :: blocks_rev }

block:
  | at=AT type_kw=TYPE instrs=c_block_type_opt {
      let span =
        merge_spans
          [ token_span at
          ; token_span type_kw
          ; span_of_nodes instrs
          ]
      in
      mk ?span
        (Type_block
           { type_block_instructions = instrs })
    }
  | at=AT complex=complex locals=c_block_local_opt {
      let span =
        merge_spans
          [ token_span at
          ; node_span complex
          ; span_of_nodes locals
          ]
      in
      mk ?span
        (Complex_block
           { complex_block_complex = complex
           ; complex_block_locals = locals })
    }

c_block_type_opt:
  | { [] }
  | items=c_instr_type_list { items }

c_instr_type_list:
  | item=c_instr_type rest=c_instr_type_list_tail { item :: rest }

c_instr_type_list_tail:
  | COMMA item=c_instr_type rest=c_instr_type_list_tail { item :: rest }
  | { [] }

c_block_local_opt:
  | { [] }
  | items=c_instr_local_list { items }

c_instr_local_list:
  | item=c_instr_local rest=c_instr_local_list_tail { item :: rest }

c_instr_local_list_tail:
  | COMMA item=c_instr_local rest=c_instr_local_list_tail { item :: rest }
  | { [] }

c_instr_type:
  | generator=generator has_value=HAS_VALUE definition=complex {
      let span =
        merge_spans
          [ node_span generator
          ; token_span has_value
          ; node_span definition
          ]
      in
      let gtype =
        mk ?span
          { generator_type_generator = generator
          ; generator_type_definition = definition
          }
      in
      mk ?span (CI_type_generator gtype)
    }
  | dnamer=dnamer {
      let span = dnamer.span in
      mk ?span (CI_type_dnamer dnamer)
    }
  | mnamer=mnamer {
      let span = mnamer.span in
      mk ?span (CI_type_mnamer mnamer)
    }
  | include_stmt=include_statement {
      let span = include_stmt.span in
      mk ?span (CI_type_include include_stmt)
    }

c_instr:
  | generator=generator {
      let span = generator.span in
      mk ?span (CI_generator generator)
    }
  | dnamer=dnamer {
      let span = dnamer.span in
      mk ?span (CI_dnamer dnamer)
    }
  | mnamer=mnamer {
      let span = mnamer.span in
      mk ?span (CI_mnamer mnamer)
    }
  | include_stmt=include_statement {
      let span = include_stmt.span in
      mk ?span (CI_include include_stmt)
    }
  | attach_stmt=attach_statement {
      let span = attach_stmt.span in
      mk ?span (CI_attach attach_stmt)
    }

c_instr_local:
  | dnamer=dnamer {
      let span = dnamer.span in
      mk ?span (CI_local_dnamer dnamer)
    }
  | mnamer=mnamer {
      let span = mnamer.span in
      mk ?span (CI_local_mnamer mnamer)
    }
  | assertion=assert_statement {
      let span = assertion.span in
      mk ?span (CI_local_assert assertion)
    }

generator:
  | name=name opt_boundaries=generator_boundaries_opt {
      let boundaries_opt, spans =
        match opt_boundaries with
        | None -> (None, [])
        | Some (colon, boundaries) ->
            ( Some boundaries
            , [ token_span colon
              ; node_span boundaries
              ] )
      in
      let span =
        merge_spans (name.span :: spans)
      in
      mk ?span
        { generator_name = name
        ; generator_boundaries = boundaries_opt
        }
    }

generator_boundaries_opt:
  | { None }
  | colon=COLON boundaries=boundaries { Some (colon, boundaries) }

boundaries:
  | source=diagram arrow=ARROW target=diagram {
      let span =
        merge_spans
          [ node_span source
          ; token_span arrow
          ; node_span target
          ]
      in
      mk ?span
        { boundaries_source = source
        ; boundaries_target = target
        }
    }

complex:
  | address_opt=option(address) lbrace=LBRACE body=c_block_opt rbrace=RBRACE {
      let span =
        merge_spans
          [ option_span address_opt
          ; token_span lbrace
          ; span_of_nodes body
          ; token_span rbrace
          ]
      in
      mk ?span
        { complex_address = address_opt
        ; complex_body = body
        }
    }

c_block_opt:
  | { [] }
  | items=c_instr_list { items }

c_instr_list:
  | item=c_instr rest=c_instr_list_tail { item :: rest }

c_instr_list_tail:
  | COMMA item=c_instr rest=c_instr_list_tail { item :: rest }
  | { [] }

address:
  | first=name rest=address_rest {
      let segments = first :: rest in
      let span = span_of_nodes segments in
      mk ?span { address_segments = segments }
    }

address_rest:
  | DOT name=name rest=address_rest { name :: rest }
  | { [] }

morphism:
  | first=m_comp rest=morphism_rest {
      let components = first :: rest in
      let span = span_of_nodes components in
      mk ?span { morphism_components = components }
    }

morphism_rest:
  | DOT comp=m_comp rest=morphism_rest { comp :: rest }
  | { [] }

m_comp:
  | term=mterm {
      let span = term.span in
      mk ?span (M_component_term term)
    }
  | name=name {
      let span = name.span in
      mk ?span (M_component_name name)
    }

mterm:
  | lparen=LPAREN ext=mext of_shape=OF_SHAPE target=complex rparen=RPAREN {
      let span =
        merge_spans
          [ token_span lparen
          ; node_span ext
          ; token_span of_shape
          ; node_span target
          ; token_span rparen
          ]
      in
      mk ?span
        { mterm_ext = ext
        ; mterm_target = target
        }
    }

mext:
  | root_opt=option(mroot) lbracket=LBRACKET block=m_block rbracket=RBRACKET {
      let span =
        merge_spans
          [ option_span root_opt
          ; token_span lbracket
          ; node_span block
          ; token_span rbracket
          ]
      in
      mk ?span
        { mext_root = root_opt
        ; mext_block = block
        }
    }

mroot:
  | name=name {
      let span = name.span in
      mk ?span (M_root_name name)
    }
  | lp=LPAREN comp=m_comp dot=DOT morph=morphism rp=RPAREN {
      let span =
        merge_spans
          [ token_span lp
          ; node_span comp
          ; token_span dot
          ; node_span morph
          ; token_span rp
          ]
      in
      mk ?span
        (M_root_compose
           { mroot_component = comp
           ; mroot_morphism = morph })
    }

m_block:
  | items=m_instr_list {
      let span = span_of_nodes items in
      mk ?span { mblock_instructions = items }
    }

m_instr_list:
  | item=m_instr rest=m_instr_list_tail { item :: rest }

m_instr_list_tail:
  | COMMA item=m_instr rest=m_instr_list_tail { item :: rest }
  | { [] }

m_instr:
  | address=address maps=MAPS_TO diagram=diagram {
      let span =
        merge_spans
          [ node_span address
          ; token_span maps
          ; node_span diagram
          ]
      in
      mk ?span
        (M_instr_diagram
           { minstr_address = address
           ; minstr_diagram = diagram })
    }

mdef:
  | ext=mext {
      let span = ext.span in
      mk ?span (Mdef_ext ext)
    }
  | morphism=morphism {
      let span = morphism.span in
      mk ?span (Mdef_morphism morphism)
    }

mnamer:
  | let_kw=LET name=name of_shape=OF_SHAPE address=address equal=EQUAL definition=mdef {
      let span =
        merge_spans
          [ token_span let_kw
          ; node_span name
          ; token_span of_shape
          ; node_span address
          ; token_span equal
          ; node_span definition
          ]
      in
      mk ?span
        { mnamer_name = name
        ; mnamer_address = address
        ; mnamer_definition = definition
        }
    }

dnamer:
  | let_kw=LET name=name opt_boundaries=dnamer_boundaries_opt equal=EQUAL body=diagram {
      let boundaries_opt, spans =
        match opt_boundaries with
        | None -> (None, [])
        | Some (colon, boundaries) ->
            ( Some boundaries
            , [ token_span colon
              ; node_span boundaries
              ] )
      in
      let span =
        merge_spans
          ( [ token_span let_kw
            ; node_span name
            ]
          @ spans
          @ [ token_span equal
            ; node_span body
            ] )
      in
      mk ?span
        { dnamer_name = name
        ; dnamer_boundaries = boundaries_opt
        ; dnamer_body = body
        }
    }

dnamer_boundaries_opt:
  | { None }
  | colon=COLON boundaries=boundaries { Some (colon, boundaries) }

include_statement:
  | include_kw=INCLUDE address=address alias_opt=include_alias_opt {
      let alias_opt, spans =
        match alias_opt with
        | None -> (None, [])
        | Some (as_kw, alias) ->
            ( Some alias
            , [ token_span as_kw
              ; node_span alias
              ] )
      in
      let span =
        merge_spans
          ( [ token_span include_kw
            ; node_span address
            ]
          @ spans )
      in
      mk ?span
        { include_address = address
        ; include_alias = alias_opt
        }
    }

include_alias_opt:
  | { None }
  | as_kw=AS alias=name { Some (as_kw, alias) }

attach_statement:
  | attach_kw=ATTACH name=name of_shape=OF_SHAPE address=address along_opt=attach_along_opt {
      let along_opt, spans =
        match along_opt with
        | None -> (None, [])
        | Some (along_kw, definition) ->
            ( Some definition
            , [ token_span along_kw
              ; node_span definition
              ] )
      in
      let span =
        merge_spans
          ( [ token_span attach_kw
            ; node_span name
            ; token_span of_shape
            ; node_span address
            ]
          @ spans )
      in
      mk ?span
        { attach_name = name
        ; attach_address = address
        ; attach_along = along_opt
        }
    }

attach_along_opt:
  | { None }
  | along_kw=ALONG definition=mdef { Some (along_kw, definition) }

assert_statement:
  | assert_kw=ASSERT left=diagram equal=EQUAL right=diagram {
      let span =
        merge_spans
          [ token_span assert_kw
          ; node_span left
          ; token_span equal
          ; node_span right
          ]
      in
      mk ?span
        { assert_left = left
        ; assert_right = right
        }
    }

diagram:
  | acc=diagram_acc { diagram_from_acc acc }

diagram_acc:
  | concat=d_concat { diagram_acc_init concat }
  | acc=diagram_acc paste=PASTE nat=nat concat=d_concat {
      diagram_acc_extend acc paste nat concat
    }

d_concat:
  | first=d_expr rest=d_concat_tail {
      let exprs = first :: rest in
      let span = span_of_nodes exprs in
      mk ?span { d_concat_exprs = exprs }
    }

d_concat_tail:
  | expr=d_expr rest=d_concat_tail { expr :: rest }
  | { [] }

d_expr:
  | first=d_comp rest=d_expr_rest {
      let components = first :: rest in
      let span = span_of_nodes components in
      mk ?span { d_expr_components = components }
    }

d_expr_rest:
  | DOT comp=d_comp rest=d_expr_rest { comp :: rest }
  | { [] }

d_comp:
  | mterm=mterm {
      let span = mterm.span in
      mk ?span (D_comp_mterm mterm)
    }
  | dterm=d_term {
      let span = dterm.span in
      mk ?span (D_comp_dterm dterm)
    }
  | name=name {
      let span = name.span in
      mk ?span (D_comp_name name)
    }
  | bd=bd {
      let span = bd.span in
      mk ?span (D_comp_bd bd)
    }
  | hole=HOLE {
      let span = token_span hole in
      mk ?span D_comp_hole
    }

d_term:
  | lp=LPAREN diagram=diagram paste=PASTE nat=nat concat=d_concat rp=RPAREN {
      let span =
        merge_spans
          [ token_span lp
          ; node_span diagram
          ; token_span paste
          ; node_span nat
          ; node_span concat
          ; token_span rp
          ]
      in
      mk ?span
        (D_term_indexed
           { dterm_diagram = diagram
           ; dterm_index = nat
           ; dterm_tail = concat
           })
    }
  | lp=LPAREN concat=d_concat expr=d_expr rp=RPAREN {
      let span =
        merge_spans
          [ token_span lp
          ; node_span concat
          ; node_span expr
          ; token_span rp
          ]
      in
      mk ?span
        (D_term_pair
           { dterm_concat = concat
           ; dterm_expr = expr
           })
    }

bd:
  | token=IN { make_bd token }
  | token=OUT { make_bd token }

name:
  | token=IDENT { make_name token }

nat:
  | token=NAT { make_nat token }

option(X):
  | x=X { Some x }
  | { None }
