#!/bin/sh
# Auto-generated
set -eu
alias awk="${AWK:-awk}"
alias grep="${GREP:-grep}"
alias sed="${SED:-sed}"
input="$(cat)"
command=$(echo "$input" | awk '/^\(command .*\)$/ {sub(/^\([a-z.-]+ "/, "", $0); sub(/".*/, "", $0); print $0}')
scheme_id=$(echo "$input" | awk '/^\(scheme\.id .*\)$/ {sub(/^\([a-z.-]+ /, "", $0); sub(/\)$/, "", $0); print $0}')
languages=$(echo "$input" | awk '/^\(languages .*\)$/ {sub(/^\([a-z.-]+ /, "", $0); sub(/\)$/, "", $0); print $0}')
scheme_features=$(echo "$input" | awk '/^\(scheme\.features .*\)$/ {sub(/^\([a-z.-]+ /, "", $0); sub(/\)$/, "", $0); print $0}')
c_type_bits_int=$(echo "$input" | awk '/^\(c\.type-bits .*\(int .*\).*\)$/ {sub(/.*\(int /, "", $0); sub(/\).*/, "", $0); print $0}')
c_type_bits_long=$(echo "$input" | awk '/^\(c\.type-bits .*\(long .*\).*\)$/ {sub(/.*\(long /, "", $0); sub(/\).*/, "", $0); print $0}')
c_type_bits_float=$(echo "$input" | awk '/^\(c\.type-bits .*\(float .*\).*\)$/ {sub(/.*\(float /, "", $0); sub(/\).*/, "", $0); print $0}')
c_type_bits_double=$(echo "$input" | awk '/^\(c\.type-bits .*\(double .*\).*\)$/ {sub(/.*\(double /, "", $0); sub(/\).*/, "", $0); print $0}')
c_type_bits_pointer=$(echo "$input" | awk '/^\(c\.type-bits .*\(pointer .*\).*\)$/ {sub(/.*\(pointer /, "", $0); sub(/\).*/, "", $0); print $0}')
c_type_bits_size_t=$(echo "$input" | awk '/^\(c\.type-bits .*\(size_t .*\).*\)$/ {sub(/.*\(size_t /, "", $0); sub(/\).*/, "", $0); print $0}')
c_version=$(echo "$input" | awk '/^\(c\.version .*\)$/ {sub(/^\([a-z.-]+ "/, "", $0); sub(/".*/, "", $0); print $0}')
c_compile=$(echo "$input" | awk '/^\(c\.compile .*\)$/ {sub(/^\([a-z.-]+ /, "", $0); sub(/\)$/, "", $0); print $0}')
c_link=$(echo "$input" | awk '/^\(c\.link .*\)$/ {sub(/^\([a-z.-]+ /, "", $0); sub(/\)$/, "", $0); print $0}')
build_configure=$(echo "$input" | awk '/^\(build\.configure .*\)$/ {sub(/^\([a-z.-]+ /, "", $0); sub(/\)$/, "", $0); print $0}')
build_git_tag=$(echo "$input" | awk '/^\(build\.git\.tag .*\)$/ {sub(/^\([a-z.-]+ "/, "", $0); sub(/".*/, "", $0); print $0}')
build_git_branch=$(echo "$input" | awk '/^\(build\.git\.branch .*\)$/ {sub(/^\([a-z.-]+ "/, "", $0); sub(/".*/, "", $0); print $0}')
build_git_commit=$(echo "$input" | awk '/^\(build\.git\.commit .*\)$/ {sub(/^\([a-z.-]+ "/, "", $0); sub(/".*/, "", $0); print $0}')
build_git_modified=$(echo "$input" | awk '/^\(build\.git\.modified .*\)$/ {sub(/^\([a-z.-]+ /, "", $0); sub(/\)$/, "", $0); print $0}')
build_date=$(echo "$input" | awk '/^\(build\.date .*\)$/ {sub(/^\([a-z.-]+ "/, "", $0); sub(/".*/, "", $0); print $0}')
image_date=$(echo "$input" | awk '/^\(image\.date .*\)$/ {sub(/^\([a-z.-]+ "/, "", $0); sub(/".*/, "", $0); print $0}')
image_file=$(echo "$input" | awk '/^\(image\.file .*\)$/ {sub(/^\([a-z.-]+ "/, "", $0); sub(/".*/, "", $0); print $0}')
install_dir=$(echo "$input" | awk '/^\(install-dir .*\)$/ {sub(/^\([a-z.-]+ "/, "", $0); sub(/".*/, "", $0); print $0}')
scheme_path=$(echo "$input" | awk '/^\(scheme\.path .*\)$/ {sub(/^\([a-z.-]+ /, "", $0); sub(/\)$/, "", $0); print $0}')
release=$(echo "$input" | awk '/^\(release .*\)$/ {sub(/^\([a-z.-]+ "/, "", $0); sub(/".*/, "", $0); print $0}')
release_date=$(echo "$input" | awk '/^\(release\.date .*\)$/ {sub(/^\([a-z.-]+ "/, "", $0); sub(/".*/, "", $0); print $0}')
release_name=$(echo "$input" | awk '/^\(release\.name .*\)$/ {sub(/^\([a-z.-]+ "/, "", $0); sub(/".*/, "", $0); print $0}')
website=$(echo "$input" | awk '/^\(website .*\)$/ {sub(/^\([a-z.-]+ "/, "", $0); sub(/".*/, "", $0); print $0}')
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
