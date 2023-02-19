use serde::{Deserialize, Serialize};

use crate::game::{Action, Error, TnEGame};

#[derive(Debug, Serialize, Deserialize)]
pub struct HistoryBuffer {
    pub history: Vec<TnEGame>,
}

impl HistoryBuffer {
    pub fn new() -> Self {
        Self { history: vec![TnEGame::new()] }
    }

    pub fn empty() -> Self {
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

    pub fn save(&self, path: &str) -> Result<(), std::io::Error> {
        let file = std::fs::File::create(path)?;
        let mut writer = std::io::BufWriter::new(file);
        serde_json::to_writer_pretty(&mut writer, &self.history)?;
        Ok(())
    }

    pub fn load(path: &str) -> Result<Self, std::io::Error> {
        let file = std::fs::File::open(path)?;
        let reader = std::io::BufReader::new(file);
        let history = serde_json::from_reader(reader)?;
        Ok(Self { history })
    }

    pub(crate) fn last_mut(&mut self) -> &mut TnEGame {
        self.history.last_mut().unwrap()
    }
}
