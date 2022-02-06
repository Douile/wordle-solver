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

#[derive(Debug)]
enum Rule {
    Found(HashSet<usize>, HashSet<usize>),
    Unfound(HashSet<usize>),
    Banned,
}
type CharCount = [u64; 26];

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

fn sort_and_find_best_words<'a>(
    wordlist: &'a Vec<String>,
    char_counts: &CharCount,
) -> Vec<(&'a String, u64)> {
    let mut words_with_scores: Vec<(&String, u64)> = wordlist
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
                            acc + char_counts[i]
                        }
                    } else {
                        acc
                    }
                }),
            )
        })
        .collect();
    words_with_scores.sort_by(|a, b| b.1.cmp(&a.1));

    words_with_scores
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

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let wordlist = fs::OpenOptions::new()
        .read(true)
        .open(std::env::args().next_back().unwrap())?;
    let mut reader = BufReader::new(wordlist);

    let (mut words, mut char_counts) = read_words(&mut reader);

    let words_with_scores = sort_and_find_best_words(&words, &char_counts);

    print_best_words(words_with_scores.iter());

    let mut rules: HashMap<char, Rule> = HashMap::new();
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
                if let Some(Rule::Unfound(set)) = rules.get_mut(&c) {
                    set.insert(index);
                } else {
                    rules.insert(c, Rule::Unfound([index].into()));
                }
            } else if c >= 'A' && c <= 'Z' {
                let c = c.to_ascii_lowercase();
                let not_pos = if let Some(Rule::Unfound(pos)) = rules.remove(&c) {
                    pos
                } else {
                    HashSet::new()
                };
                rules.insert(c, Rule::Found([index].into(), not_pos));
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
