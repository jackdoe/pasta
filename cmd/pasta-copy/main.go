package main

import (
	"os"

	"github.com/jackdoe/pasta/pkg/util"
)

func main() {
	h := util.Client()
	_, err := h.Post("http://unix/copy", "octet/stream", os.Stdin)
	if err != nil {
		panic(err)
	}
}
