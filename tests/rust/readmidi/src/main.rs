use std::path::Path;
use std::time::Duration;


fn file_to_bytes(path: &Path) -> Vec<u8> {
    use std::io::Read;
    use std::io::BufReader;
    use std::fs::File;
    let f = File::open(path).unwrap();
    let mut reader = BufReader::new(f);
    let mut buffer = Vec::new();
    reader.read_to_end(&mut buffer).unwrap();
    buffer
}

fn parse_midi_file(bytes: &Vec<u8>) -> (String, Duration) {
    let sw = stopwatch::Stopwatch::start_new();
    let midifile = midi_file::MidiFile::read(bytes.as_slice());
    let elapsed = sw.elapsed();
    (
        match midifile {
            Ok(_midifile) => String::from("ok")
            , Err(e) =>{
                use snafu::{ ErrorCompat };
                if let Some(backtrace) = ErrorCompat::backtrace(&e) {
                    format!("{}", backtrace)
                }else {
                    String::from("e")
                }
            }
        }
    ,elapsed)
}

fn parse_midly(bytes: &Vec<u8>) -> (String, Duration) {
    let sw = stopwatch::Stopwatch::start_new();
    let midifile = midly::Smf::parse(bytes);
    let elapsed = sw.elapsed();
    (
        match midifile {
            Ok(_midifile) => String::from("ok")
            , Err(e) => {
                format!("{}", e)
            }
        }
        ,elapsed)
}

fn parse_rimd(bytes: &mut Vec<u8>) -> (String, Duration) {
    let sw = stopwatch::Stopwatch::start_new();
    let mut bytes = bytes.as_slice();
    let midifile = rimd::SMF::from_reader(&mut bytes);
    let elapsed = sw.elapsed();
    (
        match midifile {
            Ok(_midifile) => String::from("ok")
            , Err(e) => {
                format!("{}", e)
            }
        }
        ,elapsed)
}

use walkdir::WalkDir;
fn main() {
    fn to_ms (e: Duration) -> String {
        format!("{:.4}", e.as_secs_f64()*1000.)
    }
    let f = Path::new(file!());
    let dir = f.parent().unwrap().to_str().unwrap();
    let dir = format!("{}/{}/{}",env!("CARGO_MANIFEST_DIR"), dir, "../../../../data/");
    let csv_file = format!("{}{}", &dir, "rust.csv");
    println!("{}", csv_file);
    let mut writer = csv::Writer::from_path(csv_file).unwrap();
    writer.write_record(&[
        "filename"
        ,"elapsed midi_file"
        ,"elapsed midly"
        ,"elapsed rimd"
        ,"is ok midi_file"
        ,"is ok midly"
        ,"is ok rimd"
    ]).unwrap();

    for entry in WalkDir::new(&dir) {
        let path = entry.unwrap();
        let (_, trimmed_path) = path.path().to_str().unwrap().split_at(dir.len());
        let ext = path.path().extension().unwrap().to_str();
        let is_midi_file = ext == Some("mid") || ext == Some("midi");
        if path.file_type().is_file() && is_midi_file
        {
            let mut bytes = file_to_bytes(path.path());
            let (result_midi_file,elapsed_midi_file) = parse_midi_file(&bytes);
            let (result_midly,elapsed_midly) = parse_midly(&bytes);
            let (result_rimd,elapsed_rimd) = parse_rimd(&mut bytes);

            writer.write_record(&[
                trimmed_path
                , &to_ms(elapsed_midi_file)
                , &to_ms(elapsed_midly)
                , &to_ms(elapsed_rimd)
                , result_midi_file.as_str()
                , result_midly.as_str()
                , result_rimd.as_str()
            ]).unwrap();
        }
    }
}
