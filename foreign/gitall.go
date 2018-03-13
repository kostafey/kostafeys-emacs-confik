// go get github.com/fatih/color

package main

import (
	"fmt"
	"github.com/fatih/color"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
)

func help() {
	fmt.Printf(`Gitall - handle many git repositories project case.
Apply for all repos in subfolders and all branches.

Usage:
  gitall <action> <params> <search>

Actions:
  log      - Show log commits filtered by commit message with <search> substring.
  diff     - Show diffs for all commits filtered by commit message with
			 <search> substring.
  help     - Show this help.
  barnch   - Show branches list and current branch.
  checkout - Change current branch.
  pull
  status

Params:
  -a
  -all    - Apply to all subfolders (single nested level).
  -l
  --local - Apply to current folder.

Examples:
  gitall checkout -b develop
		  Checkout to develop branch, keep all changes.
`)
}

func getExPath() string {
	f, _ := os.Open(".")
	return f.Name()
}

func diffColorizePrint(result string) {
	for _, line := range strings.Split(
		strings.TrimSuffix(result, "\n"), "\n") {
		if len(line) > 0 {
			if line[0] == '-' {
				color.Red(line)
			} else if line[0] == '+' {
				color.Green(line)
			} else {
				fmt.Printf(line)
				fmt.Printf("\n")
			}
		}
	}
}

func run(dir string, name string, arg ...string) string {
	cmd := exec.Command(name, arg...)
	cmd.Dir = dir
	out, _ := cmd.Output()
	result := string(out)
	return result
}

func header(dir string) {
	fmt.Printf("---------------------------------------\n")
	color.Yellow(dir)
}

type summary struct {
	count     int
	artifacts []string
}

var s = summary{count: 0, artifacts: []string{}}

func updateSummary(dir string) {
	if !in(dir, s.artifacts) {
		s.count++
		s.artifacts = append(s.artifacts, dir)
	}
}

func affected() {
	fmt.Printf("---------------------------------------\n")
	color.Cyan("Total affected: %d", s.count)
	color.White("%s\n", strings.Join(s.artifacts[:], "\n"))
}

func isClean(dir string) bool {
	return strings.Contains(
		run(dir, "git", "status"),
		"nothing to commit, working tree clean")
}

func isGit(dir string) bool {
	result := run(dir, "git", "status")
	return !(result == "" || strings.Contains(result, "Not a git repository"))
}

func gitGrep(dir string, search string, diff string) {
	repoDir := filepath.Join(getExPath(), dir)
	result := run(repoDir, "git", "log", "--all", fmt.Sprintf("--grep=%s", search))
	if result != "" {
		updateSummary(dir)
		if isTrue(diff) {
			for _, line := range strings.Split(
				strings.TrimSuffix(result, "\n"), "\n") {
				if strings.Contains(line, "commit ") {
					commitHash := strings.Split(line, "commit ")[1]
					result := run(repoDir, "git", "show", commitHash)
					header(dir)
					diffColorizePrint(result)
				}
			}
		} else {
			header(dir)
			fmt.Printf("%s\n", result)
		}
	}
}

func branch(currentDir string, params string) {
	repoDir := filepath.Join(getExPath(), currentDir)
	result := run(repoDir, "git", "branch")
	if result != "" {
		lines := strings.Split(strings.TrimSuffix(result, "\n"), "\n")
		color.Yellow(currentDir)
		for _, line := range lines {
			if len(line) > 0 {
				if line[0] == '*' {
					color.Green(line + "\n")
				}
				if in(params, all) && line[0] != '*' {
					color.White(line + "\n")
				}
			}
		}
	}
}

func getBranch(dir string) string {
	result := run(dir, "git", "branch")
	lines := strings.Split(strings.TrimSuffix(result, "\n"), "\n")
	for _, line := range lines {
		if len(line) > 0 {
			if line[0] == '*' {
				return strings.TrimSpace(strings.Split(line, " ")[1])
			}
		}
	}
	return ""
}

func checkout(dir string, params string, branch string) {
	repoDir := filepath.Join(getExPath(), dir)
	if branch == "" {
		branch = params
		params = ""
	}
	if getBranch(repoDir) == branch {
		return
	}
	header(dir)
	isClean := isClean(repoDir)
	stashName := ""
	if isClean {
		fmt.Printf("%s\n", "nothing to commit, working tree clean")
	} else {
		stashName = time.Now().Format("2006-01-02_15:04:05")
		run("git", "stash", "save", "\""+stashName+"\"")
		fmt.Printf("Stash: save ")
		color.Red(stashName + "\n")
	}
	if params == "" {
		run(repoDir, "git", "checkout", branch)
	} else {
		run(repoDir, "git", "checkout", params, branch)
	}
	fmt.Printf("Change branch: ")
	color.Green(branch + "\n")

	if !isClean {
		fmt.Printf("Stash: apply ")
		color.Green(stashName + "\n")
	}
}

func pull(dir string) {
	repoDir := filepath.Join(getExPath(), dir)
	result := run(repoDir, "git", "pull")
	header(dir)
	fmt.Printf("%s\n", result)
}

func status(dir string) {
	status := run(dir, "git", "status")
	if !strings.Contains(status, "nothing to commit, working tree clean") {
		updateSummary(dir)
		header(dir)
		fmt.Printf("%s\n", status)
	}
}

func fixUpstream(dir string) {
	repoDir := filepath.Join(getExPath(), dir)
	branch := getBranch(repoDir)
	result := run(repoDir, "git", "branch",
		"--set-upstream-to=origin/"+branch, branch)
	header(dir)
	fmt.Printf("%s\n", result)
}

func in(val string, arr []string) bool {
	for _, v := range arr {
		if v == val {
			return true
		}
	}
	return false
}

func isTrue(s string) bool {
	return in(s, []string{"1", "t", "T", "true", "True"})
}

// T1 @unexported
type T1 struct{}

// T2 @unexported
type T2 struct{}

// T3 @unexported
type T3 struct{}

func (t T1) traverse(fu func(string)) {
	files, _ := ioutil.ReadDir("./")
	for _, f := range files {
		if f.IsDir() {
			repoDir := filepath.Join(getExPath(), f.Name())
			status := run(repoDir, "git", "status")
			if !(status == "" || strings.Contains(status, "fatal:")) {
				fu(f.Name())
			}
		}
	}
}

func (t T2) traverse(fu func(string, string), parms string) {
	files, _ := ioutil.ReadDir("./")
	for _, f := range files {
		if f.IsDir() {
			repoDir := filepath.Join(getExPath(), f.Name())
			status := run(repoDir, "git", "status")
			if !(status == "" || strings.Contains(status, "fatal:")) {
				fu(f.Name(), parms)
			}
		}
	}
}

func (t T3) traverse(fu func(string, string, string), search string, diff string) {
	files, _ := ioutil.ReadDir("./")
	for _, f := range files {
		if f.IsDir() {
			repoDir := filepath.Join(getExPath(), f.Name())
			status := run(repoDir, "git", "status")
			if !(status == "" || strings.Contains(status, "fatal:")) {
				fu(f.Name(), search, diff)
			}
		}
	}
}

var all = []string{"-a", "a", "--all"}
var local = []string{"-a", "a", "--all"}

func runForSingleRepo(params string) bool {
	return isGit(getExPath()) && !in(params, all)
}

func main() {
	if len(os.Args) < 2 {
		help()
		return
	}
	// actions "help" "log" "diff"
	action := os.Args[1]
	if action == "help" {
		help()
		return
	}
	// params
	// -a --all
	// -l --local
	var params = ""
	if len(os.Args) > 2 {
		params = os.Args[2]
	}
	var search = ""
	if len(os.Args) > 3 {
		search = os.Args[3]
	}
	if search == "" {
		search = params
	}
	switch action {
	case "log":
		if runForSingleRepo(params) {
			fmt.Printf(getExPath())
			gitGrep(getExPath(), search, "false")
		} else {
			T3{}.traverse(gitGrep, search, "false")
			affected()
		}
	case "diff":
		if runForSingleRepo(params) {
			gitGrep(getExPath(), search, "true")
		} else {
			T3{}.traverse(gitGrep, search, "true")
			affected()
		}
	case "branch":
		T2{}.traverse(branch, params)
	case "checkout":
		// search - is branch name
		T3{}.traverse(checkout, params, search)
	case "pull":
		T1{}.traverse(pull)
	case "status":
		if runForSingleRepo(params) {
			status(getExPath())
		} else {
			T1{}.traverse(status)
			affected()
		}
	case "fix-upstream":
		if runForSingleRepo(params) {
			fixUpstream(getExPath())
		} else {
			T1{}.traverse(fixUpstream)
		}
	}
}
