pub(crate) const EXIT_CODE_SUCCESS: i32 = 0;
pub(crate) const EXIT_CODE_LEXICAL_ERROR: i32 = 65;

pub(crate) type Error = Box<dyn std::error::Error + Send + Sync>;

pub(crate) fn string_token_to_literal(token: &str) -> String {
    token[1..token.len() - 1].to_string()
}
