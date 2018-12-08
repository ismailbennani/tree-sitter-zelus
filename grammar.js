// (**************************************************************************)
// (*                                                                        *)
// (*                                Zelus                                   *)
// (*               A synchronous language for hybrid systems                *)
// (*                       http://zelus.di.ens.fr                           *)
// (*                                                                        *)
// (*                            Ismail Bennani                              *)
// (*                                                                        *)
// (*  Copyright 2018. All rights reserved.                                  *)
// (*                                                                        *)
// (*  This file is distributed under the terms of the CeCILL-C licence      *)
// (*                                                                        *)
// (*  Zelus is developed in the INRIA PARKAS team.                          *)
// (*                                                                        *)
// (**************************************************************************)
// Grammar description for the synchronous language Zélus.
// This rule names are close to those defined in the Menhir grammar used by
// `zeluc`.
// This grammar should be very close to the actual grammar used by `zeluc`
// (up to  associativity/precedence rules)
// ---------------------------------------------------------------------------

'use strict';

// shamelessly stolen from (tree-sitter-c/grammar.js)[https://github.com/tree-sitter/tree-sitter-ocaml/blob/master/grammar.js]
const PREC = {
    prefix: 19,
    dot: 18,
    app: 16,
    neg: 15,
    pow: 14,
    mult: 13,
    add: 12,
    cons: 11,
    concat: 10,
    rel: 9,
    and: 8,
    or: 7,
    on: 6,
    assign: 5,
    if: 4,
    seq: 3,
    match: 2,
    automaton: 1,
    present: 1,
};

// PARSER

module.exports = grammar({
    name: 'zelus',
    extras: ($) => [/\s/, $.comment],
    // inline: ($) => [
    //     $._seq_expression,
    //     $.arrow, $.kind, $._equation_list,
    //     $._block_of_equation
    // ],
    conflicts: ($) => [
        [$.kind, $.let_declaration]
    ],
    word: ($) => $.identifier,
    rules: {
        source_file: ($) => repeat($._implementation),

        let_declaration: ($) => seq(
            'let',
            optional('static'),
            $.identifier, '=',
            $._seq_expression
        ),
        fun_declaration: ($) => seq(
            choice(
                seq(optional('atomic'), $.kind),
                seq('let', optional('atomic'),
                    optional($.kind))
            ),
            alias($.identifier, $.fun_name),
            repeat1($._simplepattern),
            '=',
            $._seq_expression,
            optional(seq(
                'where',
                optional('rec'),
                $._equation_list))
        ),
        val_declaration: ($) => seq(
            'val', $._ide, ':', $._type_expression
        ),
        _implementation: ($) => choice(
            // open
            seq('open', $.constructor),

            // type [params] name
            seq('type',
                optional($._type_params),
                alias($.identifier, $.ty_name)
            ),
            // type[params] name = decl
            seq('type',
                optional($._type_params),
                alias($.identifier, $.ty_name),
                $._type_declaration
            ),

            // let static? var
            $.let_declaration,

            // let? atomic? K? id(args...) = expr (where rec? equations...)?
            $.fun_declaration,

            // This is only correct in interface (.zli) files, since I don't want to write a second tree-sitter package for the simple syntax of .zli files, I add it here
            // TODO: write a separate tree-sitter package for .zli files
            // val x : ty
            $.val_declaration
        ),


        _seq_expression: ($) => prec.right(PREC.seq,
            separated_nonempty_list(';', $._expression)
        ),
        _infix_expression: ($) => choice(
            prec.left(PREC.assign,
                seq(
                    $._expression,
                    alias($._infx0, $.infx),
                    $._expression
                )),
            prec.left(PREC.cons,
                seq(
                    $._expression,
                    alias($._infx1, $.infx),
                    $._expression
                )),
            prec.left(PREC.add,
                seq(
                    $._expression,
                    alias($._infx2, $.infx),
                    $._expression
                )),
            prec.right(PREC.mult,
                seq(
                    $._expression,
                    alias($._infx3, $.infx),
                    $._expression
                )),
            prec.right(PREC.pow,
                seq(
                    $._expression,
                    alias($._infx4, $.infx),
                    $._expression
                ))
        ),
        _simple_expression: ($) => choice(
            $.constructor,
            $._ext_ident,
            $._atomic_constant,
            seq('(', ')'),
            seq('[', ']'),
            seq('(', $._seq_expression, ')'),
            seq('(', $._simple_expression, ':',
                $._type_expression,
                ')'),
            seq('[', $._seq_expression, ']'),
            seq('(', list_of(',', $._expression),
                ')'),
            seq('last', $._ide),
            seq('{', label_list($._label_expr),
                '}')
        ),
        present_expression: ($) => prec.right(PREC.present,
            seq(
                'present',
                optional('|'),
                present_handlers($._scondpat, $
                    ._expression),
                optional(choice(
                    seq('init',
                        $._expression),
                    seq('else',
                        $._expression,
                        optional('end'))
                ))
            )
        ),
        match_expression: ($) => prec.right(PREC.match,
            seq(
                'match',
                $._seq_expression,
                'with',
                optional('|'),
                $._match_handlers_expression,
                optional('end')
            )
        ),
        automaton_expression: ($) => prec.right(
            PREC.automaton,
            seq(
                'automaton',
                optional('|'),
                $._automaton_handlers_expression,
                optional(seq('init', $._state))
            )
        ),
        reset_expression: ($) => seq(
            'reset',
            $._seq_expression,
            'every',
            $._expression
        ),
        let_expression: ($) => prec.right(PREC.match,
            seq(
                'let',
                optional('rec'),
                $._equation_list,
                'in',
                $._seq_expression
            )
        ),
        else_expression: ($) => seq(
            'else',
            $._expression
        ),
        if_expression: ($) => prec.right(PREC.if,
            seq(
                'if',
                $._seq_expression,
                'then',
                $._seq_expression,
                $.else_expression
            )),
        _expression: ($) => choice(
            $._simple_expression,
            $._infix_expression,
            // $.automaton_expression,
            $.match_expression,
            $.present_expression,
            $.reset_expression,
            $.if_expression,
            $.let_expression,
            // init
            'init',
            // tuple
            prec.right(PREC.seq,
                seq($._expression, ',',
                    separated_nonempty_list(',',
                        $._expression)
                )
            ),
            // app
            prec.right(PREC.app,
                seq($._simple_expression,
                    repeat1($._simple_expression)
                )
            ),
            // infx
            // prfx
            prec(PREC.prefix,
                seq($.prfx, $._expression)),
            // record update
            seq(
                '{',
                $._simple_expression,
                'with',
                $._simple_expression,
                '=',
                $._expression,
                '}'
            ),
            // slice
            seq(
                $._simple_expression,
                '{',
                $._size_expression,
                '..',
                $._size_expression,
                '}'
            ),
            // concat
            seq('{', $._simple_expression, '|',
                $._simple_expression,
                '}'),
            // access
            prec(PREC.dot,
                seq($._expression, '.', '(', $._expression,
                    ')')),
            // record access
            prec(PREC.dot,
                seq($._expression, '.', $._ext_ident)
            ),
            // period
            seq('period', $._period_expression),
            // do in
            seq(
                optional($._local_list),
                'do',
                $._equation_list,
                'in',
                $._expression
            ),
            // prefix minus
            prec(PREC.neg, seq(
                alias(choice('-', '-.'), $.prfx),
                $._expression
            ))
        ),


        _automaton_handlers_equation: ($) =>
            automaton_handlers(
                optional($._equation_list)
            )($),
        _automaton_handlers_expression: ($) =>
            automaton_handlers(
                $._expression
            )($),
        _match_handlers_block_eq: ($) =>
            match_handlers(
                $._block_of_equation
            )($),
        _match_handlers_expression: ($) =>
            match_handlers(
                $._expression
            )($),


        _block_of_equation: ($) => $._simple_equation,
        do_done_block: ($) => seq(
            'do',
            optional($._equation_list),
            'done'
        ),
        _do_equation: ($) => seq(
            optional($._let_list),
            optional($._local_list),
            $.do_done_block
        ),
        reset_equation: ($) => seq(
            'reset',
            $._equation_list,
            'every',
            $._expression
        ),
        present_equation: ($) => prec.right(
            PREC.present,
            seq(
                'present',
                optional('|'),
                present_handlers($._scondpat,
                    $
                    ._block_of_equation),
                optional(
                    seq(
                        'else',
                        $._block_of_equation
                    )
                ),
                optional('end')
            )
        ),
        else_equation: ($) => seq(
            'else',
            $._block_of_equation
        ),
        if_equation: ($) => prec.right(PREC
            .if,
            seq(
                'if',
                $._seq_expression,
                'then',
                $._block_of_equation,
                optional($.else_equation)
            )
        ),
        match_equation: ($) => prec.right(
            PREC.match,
            seq(
                'match',
                $._seq_expression,
                'with',
                optional('|'),
                $._match_handlers_block_eq,
                optional('end')
            )
        ),
        automaton_equation: ($) => prec.right(
            PREC.automaton,
            seq(
                'automaton',
                optional('|'),
                $._automaton_handlers_equation,
                optional(seq('init', $._state)),
                optional('end')
            )
        ),
        last_equation: ($) => seq(
            'last',
            $._ide, '=',
            $._expression
        ),
        forall_equation: ($) => seq(
            'forall',
            list_of(',', $._index),
            block($._equation_list)($),
            optional(
                seq(
                    'initialize',
                    list_of(
                        'and',
                        $.last_equation
                    )
                )
            ),
            'done'
        ),
        der_equation: ($) => seq(
            'der',
            $._ide,
            '=',
            $._seq_expression,
            optional(seq('init',
                $._expression)),
            optional(
                seq(
                    'reset',
                    optional('|'),
                    present_handlers($._scondpat,
                        $._expression)
                )
            )
        ),
        period_equation: ($) => seq(
            'period',
            $.pattern, '=',
            $._period_expression
        ),
        next_equation: ($) => seq(
            'next',
            $._ide,
            '=',
            $._seq_expression,
            optional(seq('init',
                $._seq_expression))
        ),
        init_equation: ($) => seq('init', $
            ._ide,
            '=', $._seq_expression
        ),
        emit_equation: ($) => seq(
            'emit',
            $._ide,
            optional(seq('=', $._seq_expression))
        ),
        eq_equation: ($) => seq(
            $.pattern,
            choice('=', '+='),
            $._seq_expression
        ),
        _simple_equation: ($) => choice(
            $.automaton_equation,
            $.match_equation,
            $.if_equation,
            $.present_equation,
            $.reset_equation,
            $._do_equation,
            $.forall_equation
        ),
        _equation: ($) => choice(
            $._simple_equation,
            $.eq_equation,
            $.period_equation,
            $.der_equation,
            $.next_equation,
            $.init_equation,
            $.emit_equation,
            // eq before eq
            prec.left(PREC.seq,
                seq(
                    $._equation,
                    'before',
                    $._equation)
            )
        ),
        _equation_list: ($) => list_of(
            'and',
            $._equation),


        _period_expression: ($) => choice(
            seq('(', $._expression, ')'),
            seq('(', $._expression, '|', $._expression,
                ')')
        ),
        _scondpat: ($) => choice(
            $._simple_expression,
            seq($._simple_expression, $._simplepattern),
            seq(
                'up',
                $._simple_expression
            ),
            prec.right(PREC.and,
                seq($._scondpat, '&', $._scondpat)
            ),
            prec.right(PREC.or,
                seq($._scondpat, '|', $._scondpat)
            ),
            prec.right(PREC.on,
                seq($._scondpat, 'on', $._scondpat)
            )
        ),


        _simplepattern: ($) => choice(
            $._atomic_constant,
            seq('(', ')'),
            '_',
            seq('-', choice($.integer, $.float)),
            $.constructor,
            $._ide,
            seq('(',
                separated_nonempty_list(
                    ',', $.pattern),
                ')'),
            seq('(', $.pattern, ':', $._type_expression,
                ')'),
            seq('{', '_', '}'),
            seq('{',
                separated_nonempty_list(
                    ';', $.pattern_label),
                '}')
        ),
        pattern: ($) => choice(
            $._simplepattern,
            prec.right(PREC.match,
                seq($.pattern, 'as', $.identifier)
            ),
            prec.right(PREC.or,
                seq($.pattern, '|', $.pattern)
            ),
            prec.left(PREC.seq,
                seq($.pattern, ',',
                    separated_nonempty_list(
                        ',', $
                        .pattern))
            ),
            seq($.constructor, $._simplepattern)
        ),


        _type_params: ($) => choice(
            separated_nonempty_list(',', $.type_var),
            seq('(', $._type_params, ')')
        ),
        _constr_decl: ($) => choice(
            $.constructor,
            seq(
                $.constructor,
                'of',
                list_of('*', $._simple_type)
            )
        ),
        _type_declaration: ($) => choice(
            seq('=', list_of('|', $._constr_decl)),
            seq('=', '{', label_list($._label_type),
                '}'),
            seq('=', $._type_expression)
        ),
        _type_expression: ($) => choice(
            // ty
            $._simple_type,
            // ty * ty [* ty ...]
            seq(
                separated_nonempty_list('*',
                    $._simple_type),
                '*', $._simple_type
            ),
            // ty -> ty
            prec.right(PREC.seq,
                seq(
                    $._type_expression,
                    $.arrow,
                    $._type_expression
                )
            ),
            // (var : ty)
            seq('(', $.identifier, ':', $._type_expression,
                ')')
        ),
        _simple_type: ($) => choice(
            // 'a
            $.type_var,
            // var
            alias($._ext_ident, $.ty_name),
            // ty var
            seq($._simple_type, alias($._ext_ident,
                $.ty_name)),
            // (ty, ty [, ty...]) var
            seq('(',
                $._type_expression, ',',
                separated_nonempty_list(',',
                    $._type_expression),
                ')', alias($._ext_ident, $.ty_name)
            ),
            // ty[size]
            seq($._simple_type, '[', $._size_expression,
                ']'),
            // (ty)
            seq('(', $._type_expression,
                ')')
        ),
        _size_expression: ($) => choice(
            $.integer,
            $._ext_ident,
            prec.left(PREC.add, seq($._size_expression,
                '+', $._size_expression
            )),
            prec.left(PREC.add, seq($._size_expression,
                '-', $._size_expression
            ))
        ),


        one_let: ($) => seq(
            'let',
            optional('rec'),
            $._equation_list
        ),
        _let_list: ($) => seq(
            $.one_let,
            'in',
            optional($._let_list)
        ),

        one_local: ($) => seq(
            $._ide,
            optional(
                seq(
                    choice(
                        'default',
                        'init'
                    ),
                    $._constant)),
            optional(seq('with', $._ext_ident))
        ),
        _local_list: ($) => seq(
            'local',
            list_of(',', $.one_local),
            optional('in'),
            optional($._local_list)
        ),


        _index: ($) => choice(
            seq($._ide, 'in', $._simple_expression),
            seq($._ide, 'out', $._ide),
            seq(
                $._ide,
                'in',
                $._simple_expression,
                '..',
                $._simple_expression
            )
        ),


        _state: ($) => choice(
            $.constructor,
            seq(
                $.constructor,
                '(',
                separated_nonempty_list(',',
                    $._expression),
                ')'
            )
        ),
        _state_pat: ($) => choice(
            $.constructor,
            seq($.constructor, '(', list_of(
                    ',',
                    $.identifier),
                ')')
        ),


        _ext_ident: ($) => prec(PREC.dot,
            choice(
                $._ide,
                seq($.constructor, '.', $._ide)
            )),
        _ide: ($) => choice(
            $.identifier,
            seq('(', $.infx, ')')),


        pattern_label: ($) => seq(
            $._ext_ident, '=', $.pattern),
        _label_type: ($) => seq(
            $.identifier, ':', $._type_expression
        ),
        _label_expr: ($) => seq(
            $._ext_ident, '=', $._expression
        ),


        _atomic_constant: ($) => choice(
            $.integer,
            $.float,
            $.string,
            $.bool,
            $.char),
        _constant: ($) => choice(
            $._atomic_constant,
            $._ext_ident),

        _infx0: ($) => choice(
            'fby',
            /[\=<\>\&\|\$][\!\$\%\&\*\+\-\.\/\:<\=\>\?\@\^\|\~]*/
        ),
        _infx1: ($) => choice(
            /[\@\^][\!\$\%\&\*\+\-\.\/\:<\=\>\?\@\^\|\~]*/
        ),
        _infx2: ($) => choice(
            'lor', 'lxor', 'or',
            /[\+\-][\!\$\%\&\*\+\-\.\/\:<\=\>\?\@\^\|\~]*/
        ),
        _infx3: ($) => choice(
            'quo', 'mod', 'land',
            /[\*\/\%][\!\$\%\&\*\+\-\.\/\:<\=\>\?\@\^\|\~]*/
        ),
        _infx4: ($) => choice(
            'lsl', 'lsr', 'asr',
            /\*\*[\!\$\%\&\*\+\-\.\/\:<\=\>\?\@\^\|\~]*/
        ),
        infx: ($) => choice(
            $._infx0, $._infx1, $._infx2, $
            ._infx3,
            $._infx4
        ),

        prfx: ($) => choice(
            'up', 'atomic', 'pre', 'disc',
            'run', 'inline', 'not',
            /[\!\.\~][\!\$\%\&\*\+\-\.\/\:<\=\>\?\@\^\|\~]*/
        ),


        identifier: ($) =>
            /[a-z_][a-zA-Z0-9_']*/,
        constructor: ($) =>
            /[A-Z][a-zA-Z0-9_']*/,
        type_var: ($) =>
            /'[a-z_][a-zA-Z0-9_']*/,

        integer: ($) =>
            /(\d+|0[xX][0-9A-Za-z]+|0[oO][0-9]+|0[bB][0-9]+)/,
        float: ($) =>
            /[0-9]+\.([0-9]*)?([eE][+\-]?[0-9]+)?/,
        // shamelessly stolen from (tree-sitter-c/grammar.js)[https://github.com/tree-sitter/tree-sitter-c/blob/master/grammar.js]
        string: $ => seq(
            '"',
            repeat(choice(
                token.immediate(prec(1,
                    /[^\\"\n]+/)),
                $.escape_sequence
            )),
            '"'
        ),
        // shamelessly stolen from (tree-sitter-c/grammar.js)[https://github.com/tree-sitter/tree-sitter-c/blob/master/grammar.js]
        escape_sequence: $ => token.immediate(
            seq(
                '\\',
                choice(
                    /[^xuU]/,
                    /\d{2,3}/,
                    /x[0-9a-fA-F]{2,}/,
                    /u[0-9a-fA-F]{4}/,
                    /U[0-9a-fA-F]{8}/
                )
            )),
        char: ($) =>
            /'([^"\\]|\\[\\'ntbr]|\\[0-9]{3})'/,
        bool: ($) => choice('true', 'false'),

        kind: ($) => choice(
            'node', 'hybrid', 'discrete',
            'fun',
            'static'),
        arrow: ($) => choice(
            '->', '-A->', '-C->', '-D->',
            '-S->', '-AD->',
            '-AS->'),

        // shamelessly stolen and adapted from (tree-sitter-c/grammar.js)[https://github.com/tree-sitter/tree-sitter-c/blob/master/grammar.js]
        comment: ($) => token(seq('(*',
            /[^*]*\*+([^)*][^*]*\*+)*/,
            ')'
        )),

    }
});

function separated_nonempty_list(sep, rule) {
    return seq(rule, repeat(seq(sep, rule)));
}

function list_of(sep, rule) {
    return seq(optional(sep),
        separated_nonempty_list(sep,
            rule));
}

function label_list(rule) {
    return seq(separated_nonempty_list(';', rule),
        optional(
            ';'));
}

function present_handlers(pattern, rule) {
    return prec.right(PREC.match,
        separated_nonempty_list('|', seq(
            pattern, '->',
            rule))
    );
}

function block(rule) {
    return ($) => seq(
        optional($._let_list),
        optional($._local_list),
        'do',
        rule
    );
}

function automaton_handlers(rule) {
    function _emission($) {
        return choice(
            seq($.one_let, 'in', optional($._let_list)),
            seq(optional($._let_list),
                optional($._local_list),
                'do', optional($._equation_list),
                'in')
        );
    }

    function _then_cont($) {
        return choice('then', 'continue');
    }

    function _until_unless($) {
        return choice('until', 'unless');
    }

    function _escape($) {
        return choice(
            seq($._scondpat, _then_cont($), $._state),
            seq($._scondpat, _then_cont($),
                _emission($),
                $._state)
        );
    }

    function _escape_list($) {
        return prec.left(PREC.seq,
            separated_nonempty_list(
                'else', _escape($)
            ));
    }

    return ($) => prec.right(PREC.seq, repeat1(seq(
        $._state_pat,
        '->',
        block(rule)($),
        choice(
            'done',
            seq(_then_cont($), $._state),
            seq(_then_cont($),
                _emission($), $._state),
            seq(_until_unless($),
                _escape_list(
                    $)),
            seq('until', _escape_list($),
                'unless', _escape_list(
                    $))
        ))));
}

function match_handlers(rule) {
    return ($) =>
        present_handlers($.pattern, rule);
}