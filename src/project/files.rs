use serde::Deserialize;

#[derive(Deserialize)]
pub struct ProjectSection {
    pub name: String,
}

#[derive(Deserialize)]
pub struct APLangWorkspaceFile {
    pub project: ProjectSection,
}
