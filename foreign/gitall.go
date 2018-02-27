package main

import (
	"fmt";
	"strings";
	"os";
	"os/exec";
	"io/ioutil";
	"path/filepath";
	"github.com/fatih/color";
)

func getExPath() string {
	f, _ := os.Open(".")
	return f.Name()
}

func exec4Print(s string) {

}

func gitGrep(currentDir string, search string, diff bool) {
	repoDir := filepath.Join(getExPath(), currentDir)
	// fmt.Printf("Search for '%s' in %s\n", search, repoDir)

	cmd := exec.Command("git", "log", "--all", fmt.Sprintf("--grep=%s", search))
	cmd.Dir = repoDir
	out, _ := cmd.Output()
	result := fmt.Sprintf("%s", out)
	if result != "" {
		if diff {
			for _, line := range strings.Split(
				strings.TrimSuffix(result, "\n"), "\n") {
					if strings.Contains(line, "commit ") {
						commitHash := strings.Split(line, "commit ")[1]
						cmd := exec.Command("git", "show", commitHash)
						cmd.Dir = repoDir
						out, _ := cmd.Output()
						fmt.Printf("---------------------------------------\n")
						color.Yellow(currentDir)
						fmt.Printf("%s\n", out)
					}
				}
		} else {
			fmt.Printf("---------------------------------------\n")
			color.Yellow(currentDir)
			fmt.Printf("%s\n", out)
		}
	}
}

func in(val string, arr []string) bool {
	for _, v := range arr {
		if v == val {
			return true
		}
	}
	return false
}

func main() {
	// actions "log" "diff"
	actions := os.Args[1]
	// params
	// -a --all
	// -l --local
	params := os.Args[2]
	search := os.Args[3]
	fmt.Printf("Search for git commits with '%s'...\n", search)
	//-----
	// log
	if actions == "log" {
		if in(params, []string{"-a", "a", "--all"}) {
			files, _ := ioutil.ReadDir("./")
			for _, f := range files {
				if f.IsDir() {
					gitGrep(f.Name(), search, false)
				}
			}
		} else if in(params, []string{"-l", "l", "--local"}) {
			gitGrep(getExPath(), search, false)
		}
	} else
	//------
	// diff
	if actions == "diff" {
		if in(params, []string{"-a", "a", "--all"}) {
			files, _ := ioutil.ReadDir("./")
			for _, f := range files {
				if f.IsDir() {
					gitGrep(f.Name(), search, true)
				}
			}
		} else if in(params, []string{"-l", "l", "--local"}) {
			gitGrep(getExPath(), search, true)
		}
	}
}
