use std::ops::{Deref, DerefMut};

#[derive(Clone, Debug)]
pub struct Rule {
    pub head: Atom,
    pub body: Vec<Atom>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Atom {
    pub pred_sym: String,
    pub terms: Vec<Term>,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Term {
    Var(String),
    Sym(String),
}

#[derive(Clone, Debug)]
pub struct Program(pub Vec<Rule>);

impl Deref for Program {
    type Target = Vec<Rule>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct KnowledgeBase(pub Vec<Atom>);

impl Deref for KnowledgeBase {
    type Target = Vec<Atom>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Substitution(pub Vec<(Term, Term)>);
impl Substitution {
    pub fn lookup(&self, key: &Term) -> Option<&Term> {
        self.0.iter().find_map(|(k, v)| (k == key).then_some(v))
    }
}

impl Deref for Substitution {
    type Target = Vec<(Term, Term)>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl DerefMut for Substitution {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

pub fn solve(program: &Program) -> KnowledgeBase {
    // NOTE: We need to check range restriction
    let mut kb = KnowledgeBase::default();
    while let Some(new_kb) = immediate_consequence(program, &kb) {
        kb = new_kb
    }
    kb
}

pub fn immediate_consequence(program: &Program, kb: &KnowledgeBase) -> Option<KnowledgeBase> {
    let mut new_knowledge = vec![];
    for atom in program.iter().flat_map(|rule| eval_rule(kb, rule).0) {
        if !kb.contains(&atom) {
            new_knowledge.push(atom);
        }
    }
    if new_knowledge.is_empty() {
        None
    } else {
        Some(KnowledgeBase(
            kb.iter().chain(new_knowledge.iter()).cloned().collect(),
        ))
    }
}

pub fn eval_rule(kb: &KnowledgeBase, rule: &Rule) -> KnowledgeBase {
    KnowledgeBase(
        walk(kb, &rule.body)
            .iter()
            .map(|subs| substitute(&rule.head, subs))
            .collect(),
    )
}

pub fn walk(kb: &KnowledgeBase, atoms: &[Atom]) -> Vec<Substitution> {
    atoms
        .iter()
        .fold(vec![Substitution::default()], |all_subs, atom| {
            eval_atom(kb, atom, &all_subs)
        })
}

pub fn eval_atom(kb: &KnowledgeBase, atom: &Atom, all_subs: &[Substitution]) -> Vec<Substitution> {
    let mut new_subs = vec![];
    for substitution in all_subs {
        let down_to_earth_atom = substitute(atom, substitution);
        for entry in kb.iter() {
            if let Some(extension) = unify(&down_to_earth_atom, entry) {
                new_subs.push(Substitution(
                    substitution
                        .iter()
                        .cloned()
                        .chain(extension.0.into_iter())
                        .collect(),
                ));
            }
        }
    }
    new_subs
}

pub fn unify(a: &Atom, b: &Atom) -> Option<Substitution> {
    if a.pred_sym != b.pred_sym || a.terms.len() != b.terms.len() {
        return None;
    }
    let mut subs = Substitution::default();
    for pair in a.terms.iter().zip(b.terms.iter()) {
        match pair {
            (_, Term::Var(_)) => panic!("The second atom is assumed to be ground"),
            (v @ Term::Var(_), s @ Term::Sym(_)) => match subs.lookup(v) {
                Some(s2 @ Term::Sym(_)) if s2 != s => {
                    return None;
                }
                _ => subs.push((v.clone(), s.clone())),
            },
            (s1 @ Term::Sym(_), s2 @ Term::Sym(_)) if s1 != s2 => {
                return None;
            }
            _ => {}
        }
    }
    Some(subs)
}

pub fn substitute(atom: &Atom, substitution: &Substitution) -> Atom {
    let mut atom = atom.clone();
    let terms = atom
        .terms
        .iter()
        .map(|term| match term {
            v @ Term::Var(_) => substitution.lookup(v).cloned().unwrap_or_else(|| v.clone()),
            Term::Sym(_) => term.clone(),
        })
        .collect();
    atom.terms = terms;
    atom
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn example() {
        // adviser("Andrew Rice",     "Mistral Contrastin").
        // adviser("Andy Hopper",     "Andrew Rice").
        // adviser("Alan Mycroft",    "Dominic Orchard").
        // adviser("David Wheeler",   "Andy Hopper").
        // adviser("Rod Burstall",    "Alan Mycroft").
        // adviser("Robin Milner",    "Alan Mycroft").
        let advisers = [
            ("Andrew Rice", "Mistral Contrastin"),
            ("Andy Hopper", "Andrew Rice"),
            ("Alan Mycroft", "Dominic Orchard"),
            ("David Wheeler", "Andy Hopper"),
            ("Rod Burstall", "Alan Mycroft"),
            ("Robin Milner", "Alan Mycroft"),
        ];

        let mut rules: Vec<_> = advisers
            .iter()
            .map(|(adviser, student)| Rule {
                head: Atom {
                    pred_sym: "adviser".to_string(),
                    terms: vec![
                        Term::Sym(adviser.to_string()),
                        Term::Sym(student.to_string()),
                    ],
                },
                body: vec![],
            })
            .collect();

        // academicAncestor(X,Y) :- adviser(X,Y).
        // academicAncestor(X,Z) :- adviser(X,Y), academicAncestor(Y,Z).
        rules.extend(vec![
            Rule {
                head: Atom {
                    pred_sym: "academicAncestor".to_string(),
                    terms: vec![Term::Var("X".to_string()), Term::Var("Y".to_string())],
                },
                body: vec![Atom {
                    pred_sym: "adviser".to_string(),
                    terms: vec![Term::Var("X".to_string()), Term::Var("Y".to_string())],
                }],
            },
            Rule {
                head: Atom {
                    pred_sym: "academicAncestor".to_string(),
                    terms: vec![Term::Var("X".to_string()), Term::Var("Z".to_string())],
                },
                body: vec![
                    Atom {
                        pred_sym: "adviser".to_string(),
                        terms: vec![Term::Var("X".to_string()), Term::Var("Y".to_string())],
                    },
                    Atom {
                        pred_sym: "academicAncestor".to_string(),
                        terms: vec![Term::Var("Y".to_string()), Term::Var("Z".to_string())],
                    },
                ],
            },
        ]);

        let program = Program(rules);
        dbg!(solve(&program));
        panic!();
    }

    #[test]
    fn test_substitute() {
        let atom = Atom {
            pred_sym: "academicAncestor".to_string(),
            terms: vec![Term::Var("X".to_string()), Term::Var("Z".to_string())],
        };
        let subs = Substitution(vec![(
            Term::Var("X".to_string()),
            Term::Sym("Quinn".to_string()),
        )]);
        assert_eq!(
            Atom {
                pred_sym: "academicAncestor".to_string(),
                terms: vec![Term::Sym("Quinn".to_string()), Term::Var("Z".to_string())],
            },
            substitute(&atom, &subs)
        )
    }

    #[test]
    fn test_unify() {
        let atom1 = Atom {
            pred_sym: "academicAncestor".to_string(),
            terms: vec![Term::Var("X".to_string()), Term::Var("Y".to_string())],
        };

        let atom2 = Atom {
            pred_sym: "academicAncestor".to_string(),
            terms: vec![Term::Sym("Alice".to_string()), Term::Sym("Bob".to_string())],
        };

        assert_eq!(
            Some(Substitution(vec![
                (Term::Var("X".to_string()), Term::Sym("Alice".to_string())),
                (Term::Var("Y".to_string()), Term::Sym("Bob".to_string()))
            ])),
            unify(&atom1, &atom2)
        );
    }

    #[test]
    fn unify_different_predsym() {
        let atom1 = Atom {
            pred_sym: "Foo".to_string(),
            terms: vec![Term::Var("X".to_string()), Term::Var("Y".to_string())],
        };

        let atom2 = Atom {
            pred_sym: "Bar".to_string(),
            terms: vec![Term::Sym("Alice".to_string()), Term::Sym("Bob".to_string())],
        };

        assert_eq!(None, unify(&atom1, &atom2));
    }

    #[test]
    fn unify_conflicting() {
        let atom1 = Atom {
            pred_sym: "academicAncestor".to_string(),
            terms: vec![Term::Var("X".to_string()), Term::Var("X".to_string())],
        };

        let atom2 = Atom {
            pred_sym: "academicAncestor".to_string(),
            terms: vec![Term::Sym("Alice".to_string()), Term::Sym("Bob".to_string())],
        };

        assert_eq!(None, unify(&atom1, &atom2));
    }

    #[test]
    fn eval_atom_test() {
        let advisers = [
            ("Andrew Rice", "Mistral Contrastin"),
            ("Andy Hopper", "Andrew Rice"),
            ("Alan Mycroft", "Dominic Orchard"),
            ("David Wheeler", "Andy Hopper"),
            ("Rod Burstall", "Alan Mycroft"),
            ("Robin Milner", "Alan Mycroft"),
        ];

        let kb = KnowledgeBase(
            advisers
                .iter()
                .map(|(adviser, student)| Atom {
                    pred_sym: "adviser".to_string(),
                    terms: vec![
                        Term::Sym(adviser.to_string()),
                        Term::Sym(student.to_string()),
                    ],
                })
                .collect(),
        );

        let atom = Atom {
            pred_sym: "adviser".to_string(),
            terms: vec![Term::Var("X".to_string()), Term::Var("Y".to_string())],
        };

        let all_subs = vec![
            Substitution(vec![(
                Term::Var("X".to_string()),
                Term::Sym("David Wheeler".to_string()),
            )]),
            Substitution(vec![(
                Term::Var("Y".to_string()),
                Term::Sym("Alan Mycroft".to_string()),
            )]),
        ];

        assert_eq!(
            vec![
                Substitution(vec![
                    (
                        Term::Var("X".to_string()),
                        Term::Sym("David Wheeler".to_string()),
                    ),
                    (
                        Term::Var("Y".to_string()),
                        Term::Sym("Andy Hopper".to_string()),
                    ),
                ],),
                Substitution(vec![
                    (
                        Term::Var("Y".to_string(),),
                        Term::Sym("Alan Mycroft".to_string(),),
                    ),
                    (
                        Term::Var("X".to_string(),),
                        Term::Sym("Rod Burstall".to_string(),),
                    ),
                ],),
                Substitution(vec![
                    (
                        Term::Var("Y".to_string(),),
                        Term::Sym("Alan Mycroft".to_string(),),
                    ),
                    (
                        Term::Var("X".to_string(),),
                        Term::Sym("Robin Milner".to_string(),),
                    ),
                ],),
            ],
            eval_atom(&kb, &atom, &all_subs)
        )
    }

    #[test]
    fn eval_rule_projection() {
        let advisers = [
            ("Andrew Rice", "Mistral Contrastin"),
            ("Andy Hopper", "Andrew Rice"),
            ("Alan Mycroft", "Dominic Orchard"),
            ("David Wheeler", "Andy Hopper"),
            ("Rod Burstall", "Alan Mycroft"),
            ("Robin Milner", "Alan Mycroft"),
        ];

        let kb = KnowledgeBase(
            advisers
                .iter()
                .map(|(adviser, student)| Atom {
                    pred_sym: "adviser".to_string(),
                    terms: vec![
                        Term::Sym(adviser.to_string()),
                        Term::Sym(student.to_string()),
                    ],
                })
                .collect(),
        );

        let rule = Rule {
            head: Atom {
                pred_sym: "onlyAdvisor".to_string(),
                terms: vec![Term::Var("X".to_string())],
            },
            body: vec![Atom {
                pred_sym: "adviser".to_string(),
                terms: vec![Term::Var("X".to_string()), Term::Var("Y".to_string())],
            }],
        };

        assert_eq!(
            KnowledgeBase(
                advisers
                    .iter()
                    .map(|(adviser, _)| Atom {
                        pred_sym: "onlyAdvisor".to_string(),
                        terms: vec![Term::Sym(adviser.to_string()),],
                    })
                    .collect(),
            ),
            eval_rule(&kb, &rule)
        );
    }
}
