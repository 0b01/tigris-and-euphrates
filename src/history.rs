use serde::{Serialize, Deserialize};

use crate::game::{Error, Action, TnEGame};

#[derive(Debug, Serialize, Deserialize)]
pub struct HistoryBuffer {
    pub history: Vec<TnEGame>,
}

impl HistoryBuffer {
    pub fn new() -> Self {
        Self { history: vec![] }
    }

    pub fn push(&mut self, game: TnEGame) {
        self.history.push(game);
    }

    pub fn process(&mut self, action: Action) -> Result<(), Error> {
        let mut game = self.history.last().cloned().unwrap();
        game.process(action)?;
        self.push(game);
        Ok(())
    }

    pub fn to_file(&self, path: &str) -> Result<(), std::io::Error> {
        let file = std::fs::File::create(path)?;
        let mut writer = std::io::BufWriter::new(file);
        serde_json::to_writer_pretty(&mut writer, &self)?;
        Ok(())
    }

    pub fn from_file(path: &str) -> Result<Self, std::io::Error> {
        let file = std::fs::File::open(path)?;
        let reader = std::io::BufReader::new(file);
        let history = serde_json::from_reader(reader)?;
        Ok(Self { history })
    }
}
