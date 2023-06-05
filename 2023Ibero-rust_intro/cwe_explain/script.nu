rm --force *.md

"cwe119120121122125126190191401415416476502676762943" | fold -w3 | from csv |
  each { |cwe| just --justfile $"($env.HOME)/Justfile" gpt $"Explain if Rust solves the CWE-($cwe.cwe), clarify if it's eradicated or just mitigated. Use code blocks with comments to clarify. Be short and concise, straight to the point" |
  save $"rust_cwe_($cwe.cwe).md"
}
echo "Finished! - "
