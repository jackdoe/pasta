package main

import (
	"io/ioutil"
	"os"

	"github.com/jackdoe/pasta/pkg/util"
)

func main() {
	h := util.Client()
	r, err := h.Get("http://unix/paste")
	if err != nil {
		panic(err)
	}
	defer r.Body.Close()

	// io.Copy(os.Stdout, r.Body) confuses zsh
	// if r.Body is empty

	b, err := ioutil.ReadAll(r.Body)
	if err != nil {
		panic(err)
	}

	if len(b) > 0 {
		_, _ = os.Stdout.Write(b)
	}
}
