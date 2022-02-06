/*
MIT License

Copyright (c) 2022 Douile

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*/

use std::collections::{HashMap, HashSet};
use std::fs;
use std::io::{stdin, stdout, BufRead, BufReader, Write};

type CharCount = [u64; 26];
type WordScores<'a> = Vec<(&'a String, u64)>;

#[derive(Debug)]
struct Wordle {
    words: Vec<String>,
    char_counts: CharCount,
    rules: HashMap<char, Rule>,
}

impl Wordle {
    fn new(words: Vec<String>, char_counts: CharCount) -> Self {
        Self {
            words,
            char_counts,
            rules: HashMap::new(),
        }
    }

    fn calculate_word_scores(&self) -> WordScores {
        self.words
            .iter()
            .map(|word| {
                let mut used: [bool; 26] = [false; 26];
                (
                    word,
                    word.chars().fold(0, |acc, c| {
                        if c >= 'a' && c <= 'z' {
                            let i = c as usize - 'a' as usize;
                            if used[i] {
                                acc
                            } else {
                                used[i] = true;
                                acc + self.char_counts[i]
                            }
                        } else {
                            acc
                        }
                    }),
                )
            })
            .collect()
    }

    fn sort_word_scores(scores: &mut WordScores) {
        scores.sort_by(|a, b| b.1.cmp(&a.1));
    }

    fn calculate_sorted_word_scores(&self) -> WordScores {
        let mut word_scores = self.calculate_word_scores();
        Self::sort_word_scores(&mut word_scores);
        word_scores
    }

    fn add_rule(&mut self, character: char, rule_to_add: PrimativeRule) {
        match rule_to_add {
            PrimativeRule::Unfound(pos) => {
                if let Some(Rule::Unfound(set)) = self.rules.get_mut(&character) {
                    set.insert(pos);
                } else {
                    self.rules.insert(character, Rule::Unfound([pos].into()));
                }
            }
            PrimativeRule::Found(pos) => {
                let old_rule = self.rules.remove(&character);
                let new_rule = match old_rule {
                    Some(Rule::Found(mut is_pos, not_pos)) => {
                        is_pos.insert(pos);
                        Rule::Found(is_pos, not_pos)
                    }
                    Some(Rule::Unfound(not_pos)) => Rule::Found([pos].into(), not_pos),
                    _ => Rule::Found([pos].into(), HashSet::new()),
                };
                self.rules.insert(character, new_rule);
            }
            PrimativeRule::Banned => {
                self.rules.insert(character, Rule::Banned);
            }
        };
    }

    fn prune_wordlist(mut self) -> Self {
        let must_use_chars: Vec<(&char, Option<&HashSet<usize>>)> = self
            .rules
            .iter()
            .filter_map(|(c, v)| match v {
                Rule::Unfound(_) => Some((c, None)),
                Rule::Found(pos, _) => Some((c, Some(pos))),
                _ => None,
            })
            .collect();

        self.words = self
            .words
            .into_iter()
            .filter(|word| {
                let will_keep = word.char_indices().all(|(i, c)| match self.rules.get(&c) {
                    Some(Rule::Unfound(pos)) | Some(Rule::Found(_, pos)) => !pos.contains(&i),
                    Some(Rule::Banned) => false,
                    None => true,
                }) && must_use_chars.iter().all(|(c, pos)| {
                    if let Some(pos) = pos {
                        let chars: Vec<char> = word.chars().collect();
                        pos.iter().all(|i| chars[*i] == **c)
                    } else {
                        word.chars().any(|ch| ch == **c)
                    }
                });

                if !will_keep {
                    for c in word.chars() {
                        self.char_counts[c as usize - A_OFFSET] -= 1;
                    }
                }

                will_keep
            })
            .collect();
        self
    }
}

#[derive(Debug)]
enum Rule {
    Found(HashSet<usize>, HashSet<usize>),
    Unfound(HashSet<usize>),
    Banned,
}

#[derive(Debug)]
enum PrimativeRule {
    Found(usize),
    Unfound(usize),
    Banned,
}

const A_OFFSET: usize = 'a' as usize;

fn read_words<T: BufRead>(file: &mut T) -> (Vec<String>, CharCount) {
    let mut words = Vec::new();
    let mut char_counts = [0; 26];

    let mut line = String::new();
    while let Ok(6) = file.read_line(&mut line) {
        let l = line.trim().to_string();
        for c in l.chars() {
            if c >= 'a' && c <= 'z' {
                char_counts[c as usize - A_OFFSET] += 1;
            }
        }
        words.push(l);
        line.clear();
    }

    (words, char_counts)
}

fn print_best_words<'a, T: Iterator<Item = &'a (&'a String, u64)>>(mut sorted_words: T) {
    println!("\x1b[32;1mBest words\x1b[0m");
    if let Some((first_word, best_score)) = sorted_words.next() {
        println!("\t{}", first_word);
        for (word, _) in sorted_words.take_while(|(_, score)| score == best_score) {
            println!("\t{}", word);
        }
    } else {
        println!("None");
    }
}

fn main() -> std::io::Result<()> {
    let wordlist = fs::OpenOptions::new()
        .read(true)
        .open(std::env::args().next_back().unwrap())?;
    let mut reader = BufReader::new(wordlist);

    let (words, char_counts) = read_words(&mut reader);
    drop(reader);

    let mut game = Wordle::new(words, char_counts);

    print_best_words(game.calculate_sorted_word_scores().iter());

    let mut o = stdout();
    let mut line = String::new();
    loop {
        print!("Enter finds: ");
        o.flush()?;
        line.clear();
        stdin().read_line(&mut line)?;
        line = line.trim().to_string();

        if line.is_empty() {
            break;
        }

        for (index, c) in line.char_indices() {
            if c >= 'a' && c <= 'z' {
                game.add_rule(c, PrimativeRule::Unfound(index));
            } else if c >= 'A' && c <= 'Z' {
                game.add_rule(c.to_ascii_lowercase(), PrimativeRule::Found(index));
            }
        }

        print!("Enter bans: ");
        o.flush()?;
        line.clear();
        stdin().read_line(&mut line)?;
        line = line.trim().to_string();

        for c in line.chars() {
            if c >= 'a' && c <= 'z' {
                rules.insert(c, Rule::Banned);
            }
        }

        eprintln!("Parsed rules: {:?}", rules);

        let must_use_chars: Vec<(&char, Option<&HashSet<usize>>)> = rules
            .iter()
            .filter_map(|(c, v)| match v {
                Rule::Unfound(_) => Some((c, None)),
                Rule::Found(pos, _) => Some((c, Some(pos))),
                _ => None,
            })
            .collect();

        words = words
            .into_iter()
            .filter(|word| {
                let will_keep = word.char_indices().all(|(i, c)| match rules.get(&c) {
                    Some(Rule::Unfound(pos)) | Some(Rule::Found(_, pos)) => !pos.contains(&i),
                    Some(Rule::Banned) => false,
                    None => true,
                }) && must_use_chars.iter().all(|(c, pos)| {
                    if let Some(pos) = pos {
                        let chars: Vec<char> = word.chars().collect();
                        pos.iter().all(|i| chars[*i] == **c)
                    } else {
                        word.chars().any(|ch| ch == **c)
                    }
                });

                if !will_keep {
                    for c in word.chars() {
                        char_counts[c as usize - A_OFFSET] -= 1;
                    }
                }

                will_keep
            })
            .collect();

        print_best_words(sort_and_find_best_words(&words, &char_counts).iter());
    }

    Ok(())
}
