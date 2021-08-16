#!/usr/bin/env pwsh
# Auto-generated
$InputArray = @($Input)
$Command = ($InputArray | Select-String -Pattern '^\(command "([^"]*)"\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$SchemeId = ($InputArray | Select-String -Pattern '^\(scheme\.id (.*)\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$Languages = ($InputArray | Select-String -Pattern '^\(languages (.*)\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$SchemeFeatures = ($InputArray | Select-String -Pattern '^\(scheme\.features (.*)\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$CTypeBitsInt = ($InputArray | Select-String -Pattern '^\(c\.type-bits .*\(int (.*?)\).*\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$CTypeBitsLong = ($InputArray | Select-String -Pattern '^\(c\.type-bits .*\(long (.*?)\).*\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$CTypeBitsFloat = ($InputArray | Select-String -Pattern '^\(c\.type-bits .*\(float (.*?)\).*\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$CTypeBitsDouble = ($InputArray | Select-String -Pattern '^\(c\.type-bits .*\(double (.*?)\).*\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$CTypeBitsPointer = ($InputArray | Select-String -Pattern '^\(c\.type-bits .*\(pointer (.*?)\).*\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$CTypeBitsSize_t = ($InputArray | Select-String -Pattern '^\(c\.type-bits .*\(size_t (.*?)\).*\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$CVersion = ($InputArray | Select-String -Pattern '^\(c\.version "([^"]*)"\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$CCompile = ($InputArray | Select-String -Pattern '^\(c\.compile (.*)\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$CLink = ($InputArray | Select-String -Pattern '^\(c\.link (.*)\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$BuildConfigure = ($InputArray | Select-String -Pattern '^\(build\.configure (.*)\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$BuildGitTag = ($InputArray | Select-String -Pattern '^\(build\.git\.tag "([^"]*)"\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$BuildGitBranch = ($InputArray | Select-String -Pattern '^\(build\.git\.branch "([^"]*)"\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$BuildGitCommit = ($InputArray | Select-String -Pattern '^\(build\.git\.commit "([^"]*)"\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$BuildGitModified = ($InputArray | Select-String -Pattern '^\(build\.git\.modified (.*)\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$BuildDate = ($InputArray | Select-String -Pattern '^\(build\.date "([^"]*)"\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$ImageDate = ($InputArray | Select-String -Pattern '^\(image\.date "([^"]*)"\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$ImageFile = ($InputArray | Select-String -Pattern '^\(image\.file "([^"]*)"\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$InstallDir = ($InputArray | Select-String -Pattern '^\(install-dir "([^"]*)"\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$SchemePath = ($InputArray | Select-String -Pattern '^\(scheme\.path (.*)\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$Release = ($InputArray | Select-String -Pattern '^\(release "([^"]*)"\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$ReleaseDate = ($InputArray | Select-String -Pattern '^\(release\.date "([^"]*)"\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$ReleaseName = ($InputArray | Select-String -Pattern '^\(release\.name "([^"]*)"\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
$Website = ($InputArray | Select-String -Pattern '^\(website "([^"]*)"\)$' | ForEach-Object {$_.Matches[0].Groups[1].Value})
Write-Host "command: $Command"
Write-Host "scheme.id: $SchemeId"
Write-Host "languages: $Languages"
Write-Host "scheme.features: $SchemeFeatures"
Write-Host "c.type-bits-int: $CTypeBitsInt"
Write-Host "c.type-bits-long: $CTypeBitsLong"
Write-Host "c.type-bits-float: $CTypeBitsFloat"
Write-Host "c.type-bits-double: $CTypeBitsDouble"
Write-Host "c.type-bits-pointer: $CTypeBitsPointer"
Write-Host "c.type-bits-size_t: $CTypeBitsSize_t"
Write-Host "c.version: $CVersion"
Write-Host "c.compile: $CCompile"
Write-Host "c.link: $CLink"
Write-Host "build.configure: $BuildConfigure"
Write-Host "build.git.tag: $BuildGitTag"
Write-Host "build.git.branch: $BuildGitBranch"
Write-Host "build.git.commit: $BuildGitCommit"
Write-Host "build.git.modified: $BuildGitModified"
Write-Host "build.date: $BuildDate"
Write-Host "image.date: $ImageDate"
Write-Host "image.file: $ImageFile"
Write-Host "install-dir: $InstallDir"
Write-Host "scheme.path: $SchemePath"
Write-Host "release: $Release"
Write-Host "release.date: $ReleaseDate"
Write-Host "release.name: $ReleaseName"
Write-Host "website: $Website"
