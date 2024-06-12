use std::fs::OpenOptions;
use std::io::Write;
use std::fs::File;
use std::path::Path;
use std::time;
use obws::events::OutputState;
use obws::events::Event;
use futures::stream::StreamExt;
use core::pin::Pin;
use pin_utils::pin_mut;
use anyhow::Result;
use obws::Client;

#[tokio::main]
async fn main() -> Result<()> {
    let mut args = std::env::args();
    args.next();
    match args.next().as_deref() {
        Some("watch") => {
            let fifo_arg = args.next().expect("fifo path");
            watch(Path::new(&fifo_arg)).await?
        }
        Some("start") => start().await?,
        Some("stop") => stop().await?,
        other => println!("Wrong usage: {other:?}"),
    }
    Ok(())
}

async fn connect() -> obws::Result<Client> {
    let password = std::env::var("OBS_PASSWORD").expect("OBS_PASSWORD not set");
    let client = Client::connect("localhost", 4455, Some(password)).await?;
    eprintln!("Connected");
    Ok(client)
}

async fn start() -> obws::Result<()> {
    let client = connect().await?;
    //client.recording().start().await?;
    client.streaming().start().await?;
    Ok(())
}
async fn stop() -> obws::Result<()> {
    let client = connect().await?;
    //client.recording().stop().await?;
    client.streaming().stop().await?;
    Ok(())
}

async fn watch(fifo: &Path) -> Result<()> {

    let mut file = OpenOptions::new().write(true).open(fifo)?;
    let retry_time = time::Duration::from_secs(5);
    inactive(&mut file)?;
    loop {
        // Connect to the OBS instance through obs-websocket.
        eprintln!("Trying to connect every {} seconds..", retry_time.as_secs());
        if let Ok(Ok(client)) = tokio::time::timeout(std::time::Duration::from_secs(5), connect()).await {

            let stream = client.events()?;

            pin_mut!(stream);
            let mut mut_stream: Pin<_> = stream;

            //let active = client.recording().status().await?.active;
            let active = client.streaming().status().await?.active;
            line(&mut file, active, None)?;
            while let Some(event) = mut_stream.next().await {
                match event {
                    //Event::RecordStateChanged { active, state, .. } =>
                    Event::StreamStateChanged { active, state } =>
                        line(&mut file, active, Some(state))?,
                    _ => {}
                }
            }
            inactive(&mut file)?;
            eprintln!("OBS disconnected, trying to reconnect every {} seconds", retry_time.as_secs());
        }

        std::thread::sleep(retry_time);
    }
}

fn inactive(file: &mut File) -> std::io::Result<()> {
    writeln!(file, "OBS inactive")?;
    file.flush()
}

fn line(file: &mut File, active: bool, state: Option<OutputState>) -> std::io::Result<()> {
    let (prefix, action) = if active {
        ("On-air", "stop")
    } else {
        ("Off-air", "start")
    };

    let suffix = match state {
        Some(OutputState::Starting) => " (Starting)",
        Some(OutputState::Stopping) => " (Stopping)",
        Some(OutputState::Reconnecting) => " (Reconnecting)",
        _ => "",
    };

    writeln!(file, "<action={} {action}>{prefix}{suffix}</action>", std::env::current_exe().unwrap().display())?;
    file.flush()
}
