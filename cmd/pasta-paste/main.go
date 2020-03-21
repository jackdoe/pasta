package main

import (
	"io"
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
	_, _ = io.Copy(os.Stdout, r.Body)
}
