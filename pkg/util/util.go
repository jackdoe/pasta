package util

import (
	"context"
	"log"
	"net"
	"net/http"
	"os"
	"path"
)

func GetUnixSocketPath() string {
	h, err := os.UserHomeDir()
	if err != nil {
		panic(err)
	}

	p := path.Join(h, ".pasta")

	err = os.MkdirAll(p, 0700)
	if err != nil {
		panic(err)
	}

	err = os.Chmod(p, 0700)
	if err != nil {
		panic(err)
	}

	return path.Join(p, "sock")
}

func Create() net.Listener {
	sock := GetUnixSocketPath()
	os.Remove(sock)

	l, err := net.Listen("unix", sock)
	if err != nil {
		log.Fatal(err)
	}

	err = os.Chmod(sock, 0600)
	if err != nil {
		log.Fatal(err)
	}
	return l
}

func Client() http.Client {
	s := GetUnixSocketPath()
	return http.Client{
		Transport: &http.Transport{
			DialContext: func(_ context.Context, _, _ string) (net.Conn, error) {
				return net.Dial("unix", s)
			},
		},
	}
}
