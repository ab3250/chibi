#!/bin/sh
# Auto-generated
set -eu
alias awk="${AWK:-awk}"
alias grep="${GREP:-grep}"
alias sed="${SED:-sed}"
input="$(cat)"
command=$(echo "$input" | grep '^(command ".*".*)$' | sed -e 's@^([a-z.-]* "@@' -e 's@".*@@')
scheme_id=$(echo "$input" | grep '^(scheme\.id .*)$' | sed -e 's@^([a-z.-]* @@' -e 's@)$@@')
languages=$(echo "$input" | grep '^(languages .*)$' | sed -e 's@^([a-z.-]* @@' -e 's@)$@@')
scheme_features=$(echo "$input" | grep '^(scheme\.features .*)$' | sed -e 's@^([a-z.-]* @@' -e 's@)$@@')
c_type_bits_int=$(echo "$input" | grep '^(c\.type-bits .*(int .*).*)$' | sed -e 's@^.*(int @@' -e 's@).*@@')
c_type_bits_long=$(echo "$input" | grep '^(c\.type-bits .*(long .*).*)$' | sed -e 's@^.*(long @@' -e 's@).*@@')
c_type_bits_float=$(echo "$input" | grep '^(c\.type-bits .*(float .*).*)$' | sed -e 's@^.*(float @@' -e 's@).*@@')
c_type_bits_double=$(echo "$input" | grep '^(c\.type-bits .*(double .*).*)$' | sed -e 's@^.*(double @@' -e 's@).*@@')
c_type_bits_pointer=$(echo "$input" | grep '^(c\.type-bits .*(pointer .*).*)$' | sed -e 's@^.*(pointer @@' -e 's@).*@@')
c_type_bits_size_t=$(echo "$input" | grep '^(c\.type-bits .*(size_t .*).*)$' | sed -e 's@^.*(size_t @@' -e 's@).*@@')
c_version=$(echo "$input" | grep '^(c\.version ".*".*)$' | sed -e 's@^([a-z.-]* "@@' -e 's@".*@@')
c_compile=$(echo "$input" | grep '^(c\.compile .*)$' | sed -e 's@^([a-z.-]* @@' -e 's@)$@@')
c_link=$(echo "$input" | grep '^(c\.link .*)$' | sed -e 's@^([a-z.-]* @@' -e 's@)$@@')
build_configure=$(echo "$input" | grep '^(build\.configure .*)$' | sed -e 's@^([a-z.-]* @@' -e 's@)$@@')
build_git_tag=$(echo "$input" | grep '^(build\.git\.tag ".*".*)$' | sed -e 's@^([a-z.-]* "@@' -e 's@".*@@')
build_git_branch=$(echo "$input" | grep '^(build\.git\.branch ".*".*)$' | sed -e 's@^([a-z.-]* "@@' -e 's@".*@@')
build_git_commit=$(echo "$input" | grep '^(build\.git\.commit ".*".*)$' | sed -e 's@^([a-z.-]* "@@' -e 's@".*@@')
build_git_modified=$(echo "$input" | grep '^(build\.git\.modified .*)$' | sed -e 's@^([a-z.-]* @@' -e 's@)$@@')
build_date=$(echo "$input" | grep '^(build\.date ".*".*)$' | sed -e 's@^([a-z.-]* "@@' -e 's@".*@@')
image_date=$(echo "$input" | grep '^(image\.date ".*".*)$' | sed -e 's@^([a-z.-]* "@@' -e 's@".*@@')
image_file=$(echo "$input" | grep '^(image\.file ".*".*)$' | sed -e 's@^([a-z.-]* "@@' -e 's@".*@@')
install_dir=$(echo "$input" | grep '^(install-dir ".*".*)$' | sed -e 's@^([a-z.-]* "@@' -e 's@".*@@')
scheme_path=$(echo "$input" | grep '^(scheme\.path .*)$' | sed -e 's@^([a-z.-]* @@' -e 's@)$@@')
release=$(echo "$input" | grep '^(release ".*".*)$' | sed -e 's@^([a-z.-]* "@@' -e 's@".*@@')
release_date=$(echo "$input" | grep '^(release\.date ".*".*)$' | sed -e 's@^([a-z.-]* "@@' -e 's@".*@@')
release_name=$(echo "$input" | grep '^(release\.name ".*".*)$' | sed -e 's@^([a-z.-]* "@@' -e 's@".*@@')
website=$(echo "$input" | grep '^(website ".*".*)$' | sed -e 's@^([a-z.-]* "@@' -e 's@".*@@')
echo "command: $command"
echo "scheme.id: $scheme_id"
echo "languages: $languages"
echo "scheme.features: $scheme_features"
echo "c.type-bits-int: $c_type_bits_int"
echo "c.type-bits-long: $c_type_bits_long"
echo "c.type-bits-float: $c_type_bits_float"
echo "c.type-bits-double: $c_type_bits_double"
echo "c.type-bits-pointer: $c_type_bits_pointer"
echo "c.type-bits-size_t: $c_type_bits_size_t"
echo "c.version: $c_version"
echo "c.compile: $c_compile"
echo "c.link: $c_link"
echo "build.configure: $build_configure"
echo "build.git.tag: $build_git_tag"
echo "build.git.branch: $build_git_branch"
echo "build.git.commit: $build_git_commit"
echo "build.git.modified: $build_git_modified"
echo "build.date: $build_date"
echo "image.date: $image_date"
echo "image.file: $image_file"
echo "install-dir: $install_dir"
echo "scheme.path: $scheme_path"
echo "release: $release"
echo "release.date: $release_date"
echo "release.name: $release_name"
echo "website: $website"
