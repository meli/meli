/*
 * melib
 *
 * Copyright 2019 Manos Pitsidianakis
 *
 * This file is part of meli.
 *
 * meli is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * meli is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with meli. If not, see <http://www.gnu.org/licenses/>.
 */

#[cfg(not(test))]
use std::fs::{File, OpenOptions};
use std::{
    io::{BufWriter, Write},
    path::PathBuf,
    sync::{
        atomic::{AtomicU8, Ordering},
        Arc, Mutex,
    },
};

use log::{Level, LevelFilter, Log, Metadata, Record};

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, PartialOrd, Serialize)]
#[repr(u8)]
pub enum LogLevel {
    OFF = 0,
    ERROR,
    WARN,
    #[default]
    INFO,
    DEBUG,
    TRACE,
}

impl From<u8> for LogLevel {
    fn from(verbosity: u8) -> Self {
        match verbosity {
            0 => Self::OFF,
            1 => Self::ERROR,
            2 => Self::WARN,
            3 => Self::INFO,
            4 => Self::DEBUG,
            _ => Self::TRACE,
        }
    }
}

impl From<Level> for LogLevel {
    fn from(l: Level) -> Self {
        match l {
            Level::Error => Self::ERROR,
            Level::Warn => Self::WARN,
            Level::Info => Self::INFO,
            Level::Debug => Self::DEBUG,
            Level::Trace => Self::TRACE,
        }
    }
}

impl From<LogLevel> for Level {
    fn from(l: LogLevel) -> Self {
        match l {
            LogLevel::ERROR => Self::Error,
            LogLevel::WARN => Self::Warn,
            LogLevel::OFF | LogLevel::INFO => Self::Info,
            LogLevel::DEBUG => Self::Debug,
            LogLevel::TRACE => Self::Trace,
        }
    }
}

impl From<LevelFilter> for LogLevel {
    fn from(l: LevelFilter) -> Self {
        match l {
            LevelFilter::Off => Self::OFF,
            LevelFilter::Error => Self::ERROR,
            LevelFilter::Warn => Self::WARN,
            LevelFilter::Info => Self::INFO,
            LevelFilter::Debug => Self::DEBUG,
            LevelFilter::Trace => Self::TRACE,
        }
    }
}

impl From<LogLevel> for LevelFilter {
    fn from(l: LogLevel) -> Self {
        match l {
            LogLevel::OFF => Self::Off,
            LogLevel::ERROR => Self::Error,
            LogLevel::WARN => Self::Warn,
            LogLevel::INFO => Self::Info,
            LogLevel::DEBUG => Self::Debug,
            LogLevel::TRACE => Self::Trace,
        }
    }
}

impl std::fmt::Display for LogLevel {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                OFF => "OFF",
                ERROR => "ERROR",
                WARN => "WARN",
                INFO => "INFO",
                DEBUG => "DEBUG",
                TRACE => "TRACE",
            }
        )
    }
}

use LogLevel::*;

#[derive(Clone, Copy, Debug, Default, Deserialize, Eq, Hash, PartialEq, PartialOrd, Serialize)]
pub enum Destination {
    File,
    #[default]
    Stderr,
    None,
}

struct FileOutput {
    #[cfg(test)]
    writer: BufWriter<std::io::Stderr>,
    #[cfg(not(test))]
    writer: BufWriter<std::fs::File>,
    path: PathBuf,
}

#[derive(Clone)]
pub struct StderrLogger {
    dest: Arc<Mutex<FileOutput>>,
    level: Arc<AtomicU8>,
    print_level: bool,
    print_module_names: bool,
    debug_dest: Destination,
}

impl std::fmt::Debug for StderrLogger {
    fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result {
        fmt.debug_struct(stringify!(StderrLogger))
            .field("level", &LogLevel::from(self.level.load(Ordering::SeqCst)))
            .field("print_level", &self.print_level)
            .field("print_module_names", &self.print_module_names)
            .field("debug_dest", &self.debug_dest)
            .finish()
    }
}

impl Default for StderrLogger {
    fn default() -> Self {
        Self::new(LogLevel::default())
    }
}

impl StderrLogger {
    pub fn new(level: LogLevel) -> Self {
        use std::sync::Once;

        static INIT_STDERR_LOGGING: Once = Once::new();

        #[cfg(not(test))]
        let logger = {
            #[inline(always)]
            fn __inline_err_wrap() -> Result<(PathBuf, File), Box<dyn std::error::Error>> {
                let data_dir = xdg::BaseDirectories::with_prefix("meli")?;
                let path = data_dir.place_data_file("meli.log")?;
                let log_file = OpenOptions::new().append(true) /* writes will append to a file instead of overwriting previous contents */
                    .create(true) /* a new file will be created if the file does not yet already exist.*/
                    .read(true)
                    .open(&path)?;
                Ok((path, log_file))
            }
            let (path, log_file) =
                __inline_err_wrap().expect("Could not create log file in XDG_DATA_DIR");
            Self {
                dest: Arc::new(Mutex::new(FileOutput {
                    writer: BufWriter::new(log_file),
                    path,
                })),
                level: Arc::new(AtomicU8::new(level as u8)),
                print_level: true,
                print_module_names: true,
                debug_dest: if std::env::var("MELI_DEBUG_STDERR").is_ok() {
                    Destination::Stderr
                } else {
                    Destination::None
                },
            }
        };
        #[cfg(test)]
        let logger = {
            Self {
                dest: Arc::new(Mutex::new(FileOutput {
                    writer: BufWriter::new(std::io::stderr()),
                    path: PathBuf::new(),
                })),
                level: Arc::new(AtomicU8::new(level as u8)),
                print_level: true,
                print_module_names: true,
                debug_dest: Destination::Stderr,
            }
        };

        #[cfg(feature = "debug-tracing")]
        log::set_max_level(
            if matches!(LevelFilter::from(logger.log_level()), LevelFilter::Off) {
                LevelFilter::Off
            } else {
                LevelFilter::Trace
            },
        );
        #[cfg(not(feature = "debug-tracing"))]
        log::set_max_level(LevelFilter::from(logger.log_level()));

        INIT_STDERR_LOGGING.call_once(|| {
            log::set_boxed_logger(Box::new(logger.clone())).unwrap();
        });
        logger
    }

    pub fn log_level(&self) -> LogLevel {
        self.level.load(Ordering::SeqCst).into()
    }

    #[cfg(not(test))]
    pub fn change_log_dest(&mut self, path: PathBuf) {
        use crate::utils::shellexpand::ShellExpandTrait;

        let path = path.expand(); // expand shell stuff
        let mut dest = self.dest.lock().unwrap();
        *dest = FileOutput {
            writer: BufWriter::new(OpenOptions::new().append(true) /* writes will append to a file instead of overwriting previous contents */
                         .create(true) /* a new file will be created if the file does not yet already exist.*/
                         .read(true)
                         .open(&path).unwrap()),
            path
        };
    }

    pub fn log_dest(&self) -> PathBuf {
        self.dest.lock().unwrap().path.clone()
    }
}

impl Log for StderrLogger {
    fn enabled(&self, metadata: &Metadata) -> bool {
        !["polling", "async_io"]
            .iter()
            .any(|t| metadata.target().starts_with(t))
            && (metadata.level() <= Level::from(self.log_level())
                || !matches!(self.debug_dest, Destination::None))
        //metadata.level() <= self.log_level_filter() &&
        // self.includes_module(metadata.target())
    }

    fn log(&self, record: &Record) {
        if !self.enabled(record.metadata()) {
            return;
        }

        fn write(
            writer: &mut impl Write,
            record: &Record,
            (print_level, print_module_names): (bool, bool),
        ) -> Option<()> {
            writer
                .write_all(
                    super::datetime::timestamp_to_string(super::datetime::now(), None, false)
                        .as_bytes(),
                )
                .ok()?;
            writer.write_all(b" [").ok()?;
            if print_level {
                writer
                    .write_all(record.level().to_string().as_bytes())
                    .ok()?;
            }
            write!(writer, "]: ").ok()?;
            if print_module_names {
                write!(writer, "{}: ", record.metadata().target()).ok()?;
            }
            write!(writer, "{}", record.args()).ok()?;
            writer.write_all(b"\n").ok()?;
            writer.flush().ok()?;
            Some(())
        }

        // if logging isn't enabled for this level do a quick out
        match (
            self.debug_dest,
            record.metadata().level() <= Level::from(self.log_level()),
        ) {
            (Destination::None, false) => {}
            (Destination::None | Destination::File, _) => {
                _ = self.dest.lock().ok().and_then(|mut d| {
                    write(
                        &mut d.writer,
                        record,
                        (self.print_level, self.print_module_names),
                    )
                });
            }
            (Destination::Stderr, true) => {
                _ = self.dest.lock().ok().and_then(|mut d| {
                    write(
                        &mut d.writer,
                        record,
                        (self.print_level, self.print_module_names),
                    )
                });
                _ = write(
                    &mut std::io::stderr(),
                    record,
                    (self.print_level, self.print_module_names),
                );
            }
            (Destination::Stderr, false) => {
                _ = write(
                    &mut std::io::stderr(),
                    record,
                    (self.print_level, self.print_module_names),
                );
            }
        }
    }

    fn flush(&self) {
        self.dest
            .lock()
            .ok()
            .and_then(|mut w| w.writer.flush().ok());
    }
}
