use serde::Deserialize;

#[derive(Deserialize)]
pub struct ProjectSection {
    name: String,
}

#[derive(Deserialize)]
pub struct APLangWorkspaceFile {
    project: ProjectSection,
}
