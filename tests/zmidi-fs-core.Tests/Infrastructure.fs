module ZMidi.Tests.Infrastructure

type ArchiveKind =
  | Zip | TarGz
  override x.ToString() =
    match x with
    | Zip -> ".zip"
    | TarGz -> ".tar.gz"

open System
open System.IO    
open System.IO.Compression
open System.Net.Http
open System.Text    
open ICSharpCode.SharpZipLib.GZip
open ICSharpCode.SharpZipLib.Tar

let extractTarGz gzArchiveName destFolder =
    use inStream = File.OpenRead gzArchiveName
    use gzipStream = new GZipInputStream(inStream)
    use tarArchive = TarArchive.CreateInputTarArchive(gzipStream, Encoding.UTF8)
    tarArchive.ExtractContents destFolder
  
let downloadFileSet (directory: DirectoryInfo) (filesetUri: string) filesetName archiveKind =
  let archiveFilename = $"{filesetName}{archiveKind}"
  directory.Create()
  if not (Directory.Exists(Path.Combine(directory.FullName, filesetName))) then
    let archive = Path.Combine(directory.FullName, archiveFilename)
    if not (File.Exists archive) then
      printfn $"download file set: {archive} doesn't exist, downloading from {filesetUri}"
      use client = new HttpClient(Timeout = TimeSpan.FromMinutes 30)
      use resp = client.Send(new HttpRequestMessage(RequestUri=Uri filesetUri))
      use stream = resp.Content.ReadAsStream()
      use file = File.OpenWrite(archive)
      stream.CopyTo(file)
      
    printfn $"extracting {archive}..."
    match archiveKind with
    | Zip -> 
      let fz = ICSharpCode.SharpZipLib.Zip.FastZip()
      fz.ExtractZip(archive, Path.Combine(directory.FullName, filesetName), "")
      // dotnet one fails one the groove dataset.
      //ZipFile.ExtractToDirectory(archive, Path.Combine(directory.FullName, filesetName))
    | TarGz -> extractTarGz archive directory.FullName
    File.Delete archive
    printfn $"done with {archive}."

