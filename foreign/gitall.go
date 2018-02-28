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
)

func getExPath() string {
    f, _ := os.Open(".")
    return f.Name()
}

func colorizePrint(s []byte) {
    result := fmt.Sprintf("%s", s)
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
                        colorizePrint(out)
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

func help() {
    fmt.Printf(`Gitall - handle many git repositories project case. 
Apply for all repos in subfolders and all branches.

Usage:
  gitall <action> <params> <search>
  
Actions:
  log  - Show log commits filtered by commit message with <search> substring.
  diff - Show diffs for all commits filtered by commit message with 
         <search> substring.
  help - Show this help.

Params:
  -a
  -all    - Apply to all subfolders (single nested level).
  -l
  --local - Apply to current folder.

`)
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
    params := os.Args[2]
    search := os.Args[3]
    fmt.Printf("Search for git commits with '%s'...\n", search)
    //-----
    // log
    if action == "log" {
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
    if action == "diff" {
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
