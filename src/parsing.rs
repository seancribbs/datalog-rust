use crate::*;
use nom::branch::alt;
use nom::bytes::complete::tag;
use nom::character::complete::{alphanumeric0, multispace0, satisfy};
use nom::combinator::opt;
use nom::error::ParseError;
use nom::multi::{many0, separated_list0, separated_list1};
use nom::sequence::{delimited, preceded};
use nom::IResult;

pub fn parse(text: &str) -> IResult<&str, Program> {
    let (text, rules) = many0(ws(rule))(text)?;
    Ok((text, Program(rules)))
}

fn rule(text: &str) -> IResult<&str, Rule> {
    let (text, head) = ws(atom)(text)?;
    let (text, body) = opt(preceded(tag(":-"), separated_list1(tag(","), ws(atom))))(text)?;
    let (text, _) = tag(".")(text)?;
    Ok((
        text,
        Rule {
            head,
            body: body.unwrap_or_default(),
        },
    ))
}

fn atom(text: &str) -> IResult<&str, Atom> {
    let (text, pred_sym0) = satisfy(|c| c.is_ascii_lowercase())(text)?;
    let (text, pred_sym_rest) = alphanumeric0(text)?;
    let pred_sym = format!("{pred_sym0}{pred_sym_rest}");

    let (remainder, terms) = opt(delimited(tag("("), term_list, tag(")")))(text)?;
    Ok((
        remainder,
        Atom {
            pred_sym,
            terms: terms.unwrap_or_default(),
        },
    ))
}

fn term_list(text: &str) -> IResult<&str, Vec<Term>> {
    separated_list0(tag(","), ws(term))(text)
}

fn term(text: &str) -> IResult<&str, Term> {
    alt((symbol, var))(text)
}

fn symbol(text: &str) -> IResult<&str, Term> {
    let (remainder, contents) =
        delimited(tag("\""), many0(satisfy(|c| c != '"')), tag("\""))(text)?;
    Ok((remainder, Term::Sym(String::from_iter(contents))))
}

fn var(text: &str) -> IResult<&str, Term> {
    let (text, initial) = satisfy(|c| c.is_ascii_uppercase())(text)?;
    let (remainder, rest) = alphanumeric0(text)?;
    Ok((remainder, Term::Var(format!("{initial}{rest}"))))
}

fn ws<'a, F: 'a, O, E: ParseError<&'a str>>(
    inner: F,
) -> impl FnMut(&'a str) -> IResult<&'a str, O, E>
where
    F: Fn(&'a str) -> IResult<&'a str, O, E>,
{
    delimited(multispace0, inner, multispace0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_symbol() {
        let expected = Term::Sym("Foo".to_string());
        let (_, result) = symbol("\"Foo\"").unwrap();
        assert_eq!(expected, result);
    }

    #[test]
    fn parse_var() {
        let expected = Term::Var("Exceptional".to_string());
        let (_, result) = var("Exceptional").unwrap();
        assert_eq!(expected, result);
    }

    #[test]
    fn parse_term() {
        let expected = Term::Sym("Foo".to_string());
        let (_, result) = term("\"Foo\"").unwrap();
        assert_eq!(expected, result);

        let expected = Term::Var("Exceptional".to_string());
        let (_, result) = term("Exceptional").unwrap();
        assert_eq!(expected, result);
    }

    #[test]
    fn parse_atom() {
        let expected = Atom {
            pred_sym: "noArgs".to_string(),
            terms: vec![],
        };
        let (_, result) = atom("noArgs").unwrap();
        assert_eq!(expected, result);

        let expected = Atom {
            pred_sym: "args".to_string(),
            terms: vec![Term::Var("X1".to_string()), Term::Var("X2".to_string())],
        };
        let (_, result) = atom("args(  X1 , X2  )").unwrap();
        assert_eq!(expected, result);
    }

    #[test]
    fn parse_rule() {
        // 1. Rule with no body
        //   a. with no terms in the head
        let expected = Rule {
            head: Atom {
                pred_sym: "noArgs".to_string(),
                terms: vec![],
            },
            body: vec![],
        };
        let (_, result) = rule("noArgs.").unwrap();
        assert_eq!(expected, result);
        //   b. with terms in the head
        let expected = Rule {
            head: Atom {
                pred_sym: "argsNoBody".to_string(),
                terms: vec![Term::Sym("A".to_string()), Term::Sym("B".to_string())],
            },
            body: vec![],
        };
        let (_, result) = rule("argsNoBody(\"A\", \"B\").").unwrap();
        assert_eq!(expected, result);
        // 2. Rule with a body
        //   a. with no terms in the head
        let expected = Rule {
            head: Atom {
                pred_sym: "noArgs".to_string(),
                terms: vec![],
            },
            body: vec![Atom {
                pred_sym: "foo".to_string(),
                terms: vec![Term::Sym("Bar".to_string())],
            }],
        };
        let (_, result) = rule("noArgs :- foo(\"Bar\").").unwrap();
        assert_eq!(expected, result);
        //   b. with terms in the head
        let expected = Rule {
            head: Atom {
                pred_sym: "hasArgs".to_string(),
                terms: vec![Term::Var("X".to_string())],
            },
            body: vec![Atom {
                pred_sym: "foo".to_string(),
                terms: vec![Term::Var("X".to_string())],
            }],
        };
        let (_, result) = rule("hasArgs(X) :- foo(X).").unwrap();
        assert_eq!(expected, result);
        //  c. with multiple terms in the head and multiple atoms in the body
        let expected = rule!(
            atom!("hasArgs", var!("X"), var!("Y")) =>
            atom!("foo", var!("X")),
            atom!("bar", var!("Y"))
        );
        let (_, result) =
            rule("\n\thasArgs(X, Y) \t\t:- \n   \t \nfoo(X),\r\n  \tbar(Y) \t.").unwrap();
        assert_eq!(expected, result);
    }
}
