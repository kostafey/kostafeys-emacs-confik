/* main.go - Golang data format wrapper for Emacs

Copyright (C) 2023 Kostafey <kostafey@gmail.com>.

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

package main

// int plugin_is_GPL_compatible;
import "C"

import (
	"github.com/mopemope/emacs-module-go"
)

func init() {
	emacs.Register(initModule)
}

func initModule(env emacs.Environment) {
	stdlib := env.StdLib()
	stdlib.Message("Foreign Golang format module init")

	env.RegisterFunction("foreign-find-go", find, 3, "native find", nil)
	env.RegisterFunction("foreign-replace-go", replace, 3, "native replace", nil)
	env.RegisterFunction("foreign-jsonpp-go", jsonpp, 1, "native jsonpp", nil)
	env.RegisterFunction("foreign-xmlpp-go", xmlpp, 1, "native xmlpp", nil)
	env.RegisterFunction("foreign-ednpp-go", ednpp, 1, "native ednpp", nil)
}

func main() {}
