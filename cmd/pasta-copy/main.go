package main

import (
	"bytes"
	"encoding/binary"
	"flag"
	"fmt"
	"io"
	"os"
	"unsafe"

	"github.com/jackdoe/pasta/pkg/util"
	"golang.org/x/sys/unix"
)

const TIOCL_GETSEL = 18
const TIOCLINUX = 0x541C

func ioctl(fd int, req uint, arg uintptr) (err error) {
	_, _, e1 := unix.Syscall(unix.SYS_IOCTL, uintptr(fd), uintptr(req), uintptr(arg))
	if e1 != 0 {
		err = fmt.Errorf("errno: %d", e1)
	}
	return
}

var nativeEndian binary.ByteOrder

func init() {
	buf := [2]byte{}
	*(*uint16)(unsafe.Pointer(&buf[0])) = uint16(0xABCD)

	switch buf {
	case [2]byte{0xCD, 0xAB}:
		nativeEndian = binary.LittleEndian
	case [2]byte{0xAB, 0xCD}:
		nativeEndian = binary.BigEndian
	default:
		nativeEndian = binary.LittleEndian
	}
}

func main() {
	sel := flag.Bool("sel", false, "send the mouse selection")
	flag.Parse()
	var data io.Reader
	data = os.Stdin
	if *sel {
		fd, err := os.Open("/dev/tty")
		if err != nil {
			panic(err)
		}
		defer fd.Close()
		value := make([]byte, 1024*10) // 10k should be enough

		value[0] = byte(TIOCL_GETSEL)

		nativeEndian.PutUint32(value[1:], uint32(len(value)-5))

		err = ioctl(int(fd.Fd()), TIOCLINUX, uintptr(unsafe.Pointer(&value[0])))
		if err != nil {
			panic(err)
		}

		size := nativeEndian.Uint32(value[1:])
		data = bytes.NewReader(value[5 : size+5])
	}

	h := util.Client()
	_, err := h.Post("http://unix/copy", "octet/stream", data)
	if err != nil {
		panic(err)
	}
}
