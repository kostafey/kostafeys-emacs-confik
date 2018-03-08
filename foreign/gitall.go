// go get github.com/fatih/color

package main

import (
	"fmt";
	"strings";
	"os";
	"os/exec";
	"io/ioutil";
	"path/filepath";
	"github.com/fatih/color";
	"time";
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
	return result;
}

func header(dir string) {
	fmt.Printf("---------------------------------------\n")
	color.Yellow(dir)
}

func gitGrep(dir string, search string, diff string) {
	repoDir := filepath.Join(getExPath(), dir)
	result := run(repoDir, "git", "log", "--all", fmt.Sprintf("--grep=%s", search))
	if result != "" {
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

func branch(currentDir string) {
	repoDir := filepath.Join(getExPath(), currentDir)
	result := run(repoDir, "git", "branch")
	if result != "" {		
		lines := strings.Split(strings.TrimSuffix(result, "\n"), "\n")
		for _, line := range lines {
			if len(line) > 0 {
				if line[0] == '*' {
					color.Yellow(currentDir)
					color.Green(line + "\n")
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
	isClean := strings.Contains(
		run(repoDir, "git", "status"),
		"nothing to commit, working tree clean")
	stashName := ""
	if isClean {		
		fmt.Printf("%s\n", "nothing to commit, working tree clean")
	} else {
		stashName = time.Now().Format("2006-01-02_15:04:05");
		run("git", "stash", "save",  "\"" + stashName + "\"")
		fmt.Printf("Stash: save ")
		color.Red(stashName + "\n")
	}

	run(repoDir, "git", "checkout", params, branch)
	fmt.Printf("Change branch: ")
	color.Green(branch + "\n")
	
	if !isClean {
		fmt.Printf("Stash: apply ")
		color.Green(stashName + "\n")
	}
}

func pull(dir string) {
	repoDir := filepath.Join(getExPath(), dir)
	result := run(repoDir, "git", "status")
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

func isTrue (s string) bool {
	return in(s, []string{"1", "t", "T", "true", "True"})
}

// T1 @unexported
type T1 struct {}
// T2 @unexported
type T2 struct {}
// T3 @unexported
type T3 struct {}

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
	//-----
	// log
	if action == "log" {
		if in(params, all) {
			T3{}.traverse(gitGrep, search, "false")
		} else if in(params, local) {
			gitGrep(getExPath(), search, "false")
		}
	} else
	//------
	// diff
	if action == "diff" {
		if in(params, all) {
			T3{}.traverse(gitGrep, search, "true")
		} else if in(params, local) {
			gitGrep(getExPath(), search, "true")
		}
	} else
	//------
	// branch
	if action == "branch" {
		T1{}.traverse(branch)
	}
	//------
	// checkout
	if action == "checkout" {
		// search - is branch name
		T3{}.traverse(checkout, params, search)
	}
	// ----
	// pull
	if action == "pull" {
		// search - is branch name
		T1{}.traverse(pull)
	}
}
