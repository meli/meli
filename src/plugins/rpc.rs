/*
 * meli - plugins
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

use super::*;

#[derive(Debug)]
pub struct RpcChannel {
    stream: UnixStream,
    session: Uuid,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub struct PluginGreeting {
    version: String,
}

impl RpcChannel {
    pub fn new(stream: UnixStream, session: &Uuid) -> Result<RpcChannel> {
        let mut ret = RpcChannel {
            stream,
            session: session.clone(),
        };
        let greeting: PluginGreeting = ret.from_read().map_err(|err| {
            MeliError::new(format!("Could not get correct plugin greeting: {}", err))
        })?;
        debug!(&greeting);
        //if greeting.version != "dev" {
        //    return Err("Plugin is not compatible with our API (dev)".into());
        //}
        ret.write_ref(&rmpv::ValueRef::String(session.to_string().as_str().into()))?;
        debug!(ret.expect_ack())?;
        Ok(ret)
    }

    pub fn expect_ack(&mut self) -> Result<()> {
        debug!("expect_ack()");
        let ack: u32 = debug!(rmp_serde::decode::from_read(&mut self.stream))
            .map_err(|_| MeliError::new("Plugin did not return ACK."))?;
        if 0x6 == ack {
            Ok(())
        } else {
            Err(MeliError::new("Plugin did not return ACK."))
        }
    }

    pub fn ack(&mut self) -> Result<()> {
        debug!("ack()");
        debug!(rmpv::encode::write_value_ref(
            &mut self.stream,
            &rmpv::ValueRef::Integer(0x6.into())
        ))
        .map_err(|err| MeliError::new(err.to_string()))?;
        let _ = self.stream.flush();
        Ok(())
    }

    pub fn write_ref(&mut self, value_ref: &rmpv::ValueRef) -> Result<()> {
        debug!("write_ref() {:?}", value_ref);
        debug!(rmpv::encode::write_value_ref(&mut self.stream, value_ref))
            .map_err(|err| MeliError::new(err.to_string()))?;
        let _ = self.stream.flush();
        Ok(())
    }

    pub fn read(&mut self) -> Result<rmpv::Value> {
        debug!("read()");
        let ret: RpcResult = debug!(rmp_serde::decode::from_read(&mut self.stream))
            .map_err(|err| MeliError::new(err.to_string()))?;
        let _ = self.stream.flush();
        self.ack()?;
        debug!("read() ret={:?}", &ret);
        ret.into()
    }

    pub fn from_read<T>(&mut self) -> Result<T>
    where
        T: core::fmt::Debug + serde::de::DeserializeOwned,
    {
        debug!("from_read()");
        let ret: Result<T> = debug!(rmp_serde::decode::from_read(&mut self.stream))
            .map_err(|err| MeliError::new(err.to_string()));
        let _ = self.stream.flush();
        self.ack()?;
        debug!("read() ret={:?}", &ret);
        ret
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
#[serde(tag = "t", content = "c")]
enum RpcResult {
    Ok(rmpv::Value),
    Err(String),
}

impl RpcResult {
    fn into(self) -> Result<rmpv::Value> {
        match self {
            RpcResult::Ok(v) => Ok(v),
            RpcResult::Err(err) => Err(MeliError::new(err)),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
#[serde(tag = "t", content = "c")]
pub enum PluginResult<T: core::fmt::Debug + Clone> {
    Ok(T),
    Err(String),
}

impl<T: core::fmt::Debug + Clone + serde::Serialize + serde::de::DeserializeOwned> Into<Result<T>>
    for PluginResult<T>
{
    fn into(self) -> Result<T> {
        match self {
            PluginResult::Ok(v) => Ok(v),
            PluginResult::Err(err) => Err(MeliError::new(err)),
        }
    }
}
