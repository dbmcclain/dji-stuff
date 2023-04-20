
#| DATE           : 6 Apr 2023 
 | USER           : davidmcclain 
 | PROCESSED FILE : /usr/local/include/libusb.h
 |#

(in-package :libusb1)

(xffi:disconnect-module :libusb :remove t)
(xffi:register-module :libusb
                     ;; :connection-style :immediate
                     :dlopen-flags t ;; non-nil needed for Mac to unload dylib on disconnect-module
                     :real-name
                     (merge-pathnames
                      #+:MAC   "libusb-1.0.0.dylib"
                      ;; #+:WIN32 "libHsIIR.dll"
                      (translate-logical-pathname "PROJECTS:LIB;xxx")))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_int8_t.h"

(xffi:define-c-typedef (int8-t (:foreign-name "int8_t")) (:signed :char))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_int16_t.h"

(xffi:define-c-typedef (int16-t (:foreign-name "int16_t")) :short)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_int32_t.h"

(xffi:define-c-typedef (int32-t (:foreign-name "int32_t")) :int)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_int64_t.h"

(xffi:define-c-typedef (int64-t (:foreign-name "int64_t")) :long-long)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/_types/_uint8_t.h"

(xffi:define-c-typedef (uint8-t (:foreign-name "uint8_t"))
                      (:unsigned :char))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/_types/_uint16_t.h"

(xffi:define-c-typedef (uint16-t (:foreign-name "uint16_t"))
                      (:unsigned :short))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/_types/_uint32_t.h"

(xffi:define-c-typedef (uint32-t (:foreign-name "uint32_t"))
                      (:unsigned :int))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/_types/_uint64_t.h"

(xffi:define-c-typedef (uint64-t (:foreign-name "uint64_t"))
                      (:unsigned :long-long))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/stdint.h"

(xffi:define-c-typedef (int-least8-t (:foreign-name "int_least8_t"))
                      int8-t)
(xffi:define-c-typedef (int-least16-t (:foreign-name "int_least16_t"))
                      int16-t)
(xffi:define-c-typedef (int-least32-t (:foreign-name "int_least32_t"))
                      int32-t)
(xffi:define-c-typedef (int-least64-t (:foreign-name "int_least64_t"))
                      int64-t)
(xffi:define-c-typedef (uint-least8-t (:foreign-name "uint_least8_t"))
                      uint8-t)
(xffi:define-c-typedef (uint-least16-t (:foreign-name "uint_least16_t"))
                      uint16-t)
(xffi:define-c-typedef (uint-least32-t (:foreign-name "uint_least32_t"))
                      uint32-t)
(xffi:define-c-typedef (uint-least64-t (:foreign-name "uint_least64_t"))
                      uint64-t)
(xffi:define-c-typedef (int-fast8-t (:foreign-name "int_fast8_t"))
                      int8-t)
(xffi:define-c-typedef (int-fast16-t (:foreign-name "int_fast16_t"))
                      int16-t)
(xffi:define-c-typedef (int-fast32-t (:foreign-name "int_fast32_t"))
                      int32-t)
(xffi:define-c-typedef (int-fast64-t (:foreign-name "int_fast64_t"))
                      int64-t)
(xffi:define-c-typedef (uint-fast8-t (:foreign-name "uint_fast8_t"))
                      uint8-t)
(xffi:define-c-typedef (uint-fast16-t (:foreign-name "uint_fast16_t"))
                      uint16-t)
(xffi:define-c-typedef (uint-fast32-t (:foreign-name "uint_fast32_t"))
                      uint32-t)
(xffi:define-c-typedef (uint-fast64-t (:foreign-name "uint_fast64_t"))
                      uint64-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/arm/_types.h"

(xffi:define-c-typedef (--int8-t (:foreign-name "__int8_t"))
                      (:signed :char))
(xffi:define-c-typedef (--uint8-t (:foreign-name "__uint8_t"))
                      (:unsigned :char))
(xffi:define-c-typedef (--int16-t (:foreign-name "__int16_t")) :short)
(xffi:define-c-typedef (--uint16-t (:foreign-name "__uint16_t"))
                      (:unsigned :short))
(xffi:define-c-typedef (--int32-t (:foreign-name "__int32_t")) :int)
(xffi:define-c-typedef (--uint32-t (:foreign-name "__uint32_t"))
                      (:unsigned :int))
(xffi:define-c-typedef (--int64-t (:foreign-name "__int64_t"))
                      :long-long)
(xffi:define-c-typedef (--uint64-t (:foreign-name "__uint64_t"))
                      (:unsigned :long-long))
(xffi:define-c-typedef (--darwin-intptr-t
                       (:foreign-name "__darwin_intptr_t"))
                      :long)
(xffi:define-c-typedef (--darwin-natural-t
                       (:foreign-name "__darwin_natural_t"))
                      (:unsigned :int))
(xffi:define-c-typedef (--darwin-ct-rune-t
                       (:foreign-name "__darwin_ct_rune_t"))
                      :int)
(xffi:define-c-typedef (--mbstate-t (:foreign-name "__mbstate_t"))
                      (:union
                       (--mbstate8 (:c-array :char 128))
                       (-mbstatel :long-long)))
(xffi:define-c-typedef (--darwin-mbstate-t
                       (:foreign-name "__darwin_mbstate_t"))
                      --mbstate-t)
(xffi:define-c-typedef (--darwin-ptrdiff-t
                       (:foreign-name "__darwin_ptrdiff_t"))
                      :long)
(xffi:define-c-typedef (--darwin-size-t
                       (:foreign-name "__darwin_size_t"))
                      (:unsigned :long))
(xffi:define-c-typedef (--darwin-va-list
                       (:foreign-name "__darwin_va_list"))
                      (:pointer :void))
(xffi:define-c-typedef (--darwin-wchar-t
                       (:foreign-name "__darwin_wchar_t"))
                      :int)
(xffi:define-c-typedef (--darwin-rune-t
                       (:foreign-name "__darwin_rune_t"))
                      --darwin-wchar-t)
(xffi:define-c-typedef (--darwin-wint-t
                       (:foreign-name "__darwin_wint_t"))
                      :int)
(xffi:define-c-typedef (--darwin-clock-t
                       (:foreign-name "__darwin_clock_t"))
                      (:unsigned :long))
(xffi:define-c-typedef (--darwin-socklen-t
                       (:foreign-name "__darwin_socklen_t"))
                      --uint32-t)
(xffi:define-c-typedef (--darwin-ssize-t
                       (:foreign-name "__darwin_ssize_t"))
                      :long)
(xffi:define-c-typedef (--darwin-time-t
                       (:foreign-name "__darwin_time_t"))
                      :long)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types.h"

(xffi:define-c-typedef (--darwin-blkcnt-t
                       (:foreign-name "__darwin_blkcnt_t"))
                      --int64-t)
(xffi:define-c-typedef (--darwin-blksize-t
                       (:foreign-name "__darwin_blksize_t"))
                      --int32-t)
(xffi:define-c-typedef (--darwin-dev-t (:foreign-name "__darwin_dev_t"))
                      --int32-t)
(xffi:define-c-typedef (--darwin-fsblkcnt-t
                       (:foreign-name "__darwin_fsblkcnt_t"))
                      (:unsigned :int))
(xffi:define-c-typedef (--darwin-fsfilcnt-t
                       (:foreign-name "__darwin_fsfilcnt_t"))
                      (:unsigned :int))
(xffi:define-c-typedef (--darwin-gid-t (:foreign-name "__darwin_gid_t"))
                      --uint32-t)
(xffi:define-c-typedef (--darwin-id-t (:foreign-name "__darwin_id_t"))
                      --uint32-t)
(xffi:define-c-typedef (--darwin-ino64-t
                       (:foreign-name "__darwin_ino64_t"))
                      --uint64-t)
(xffi:define-c-typedef (--darwin-ino-t (:foreign-name "__darwin_ino_t"))
                      --darwin-ino64-t)
(xffi:define-c-typedef (--darwin-mach-port-name-t
                       (:foreign-name "__darwin_mach_port_name_t"))
                      --darwin-natural-t)
(xffi:define-c-typedef (--darwin-mach-port-t
                       (:foreign-name "__darwin_mach_port_t"))
                      --darwin-mach-port-name-t)
(xffi:define-c-typedef (--darwin-mode-t
                       (:foreign-name "__darwin_mode_t"))
                      --uint16-t)
(xffi:define-c-typedef (--darwin-off-t (:foreign-name "__darwin_off_t"))
                      --int64-t)
(xffi:define-c-typedef (--darwin-pid-t (:foreign-name "__darwin_pid_t"))
                      --int32-t)
(xffi:define-c-typedef (--darwin-sigset-t
                       (:foreign-name "__darwin_sigset_t"))
                      --uint32-t)
(xffi:define-c-typedef (--darwin-suseconds-t
                       (:foreign-name "__darwin_suseconds_t"))
                      --int32-t)
(xffi:define-c-typedef (--darwin-uid-t (:foreign-name "__darwin_uid_t"))
                      --uint32-t)
(xffi:define-c-typedef (--darwin-useconds-t
                       (:foreign-name "__darwin_useconds_t"))
                      --uint32-t)
(xffi:define-c-typedef (--darwin-uuid-t
                       (:foreign-name "__darwin_uuid_t"))
                      (:c-array (:unsigned :char) 16))
(xffi:define-c-typedef (--darwin-uuid-string-t
                       (:foreign-name "__darwin_uuid_string_t"))
                      (:c-array :char 37))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_pthread/_pthread_types.h"

(xffi:define-c-struct (--darwin-pthread-handler-rec
                      (:foreign-name "__darwin_pthread_handler_rec")
                      (:forward-reference-p t)))
(xffi:define-c-struct (--darwin-pthread-handler-rec
                      (:foreign-name "__darwin_pthread_handler_rec"))
                     (--routine
                      (:pointer (:function ((:pointer :void)) :void)))
                     (--arg (:pointer :void))
                     (--next
                      (:pointer
                       (:struct --darwin-pthread-handler-rec))))
(xffi:define-c-struct (-opaque-pthread-attr-t
                      (:foreign-name "_opaque_pthread_attr_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 56)))
(xffi:define-c-struct (-opaque-pthread-cond-t
                      (:foreign-name "_opaque_pthread_cond_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 40)))
(xffi:define-c-struct (-opaque-pthread-condattr-t
                      (:foreign-name "_opaque_pthread_condattr_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 8)))
(xffi:define-c-struct (-opaque-pthread-mutex-t
                      (:foreign-name "_opaque_pthread_mutex_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 56)))
(xffi:define-c-struct (-opaque-pthread-mutexattr-t
                      (:foreign-name "_opaque_pthread_mutexattr_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 8)))
(xffi:define-c-struct (-opaque-pthread-once-t
                      (:foreign-name "_opaque_pthread_once_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 8)))
(xffi:define-c-struct (-opaque-pthread-rwlock-t
                      (:foreign-name "_opaque_pthread_rwlock_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 192)))
(xffi:define-c-struct (-opaque-pthread-rwlockattr-t
                      (:foreign-name "_opaque_pthread_rwlockattr_t"))
                     (--sig :long)
                     (--opaque (:c-array :char 16)))
(xffi:define-c-struct (-opaque-pthread-t
                      (:foreign-name "_opaque_pthread_t"))
                     (--sig :long)
                     (--cleanup-stack
                      (:pointer
                       (:struct --darwin-pthread-handler-rec)))
                     (--opaque (:c-array :char 8176)))
(xffi:define-c-typedef (--darwin-pthread-attr-t
                       (:foreign-name "__darwin_pthread_attr_t"))
                      (:struct -opaque-pthread-attr-t))
(xffi:define-c-typedef (--darwin-pthread-cond-t
                       (:foreign-name "__darwin_pthread_cond_t"))
                      (:struct -opaque-pthread-cond-t))
(xffi:define-c-typedef (--darwin-pthread-condattr-t
                       (:foreign-name "__darwin_pthread_condattr_t"))
                      (:struct -opaque-pthread-condattr-t))
(xffi:define-c-typedef (--darwin-pthread-key-t
                       (:foreign-name "__darwin_pthread_key_t"))
                      (:unsigned :long))
(xffi:define-c-typedef (--darwin-pthread-mutex-t
                       (:foreign-name "__darwin_pthread_mutex_t"))
                      (:struct -opaque-pthread-mutex-t))
(xffi:define-c-typedef (--darwin-pthread-mutexattr-t
                       (:foreign-name "__darwin_pthread_mutexattr_t"))
                      (:struct -opaque-pthread-mutexattr-t))
(xffi:define-c-typedef (--darwin-pthread-once-t
                       (:foreign-name "__darwin_pthread_once_t"))
                      (:struct -opaque-pthread-once-t))
(xffi:define-c-typedef (--darwin-pthread-rwlock-t
                       (:foreign-name "__darwin_pthread_rwlock_t"))
                      (:struct -opaque-pthread-rwlock-t))
(xffi:define-c-typedef (--darwin-pthread-rwlockattr-t
                       (:foreign-name "__darwin_pthread_rwlockattr_t"))
                      (:struct -opaque-pthread-rwlockattr-t))
(xffi:define-c-typedef (--darwin-pthread-t
                       (:foreign-name "__darwin_pthread_t"))
                      (:pointer (:struct -opaque-pthread-t)))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_u_int8_t.h"

(xffi:define-c-typedef (u-int8-t (:foreign-name "u_int8_t"))
                      (:unsigned :char))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_u_int16_t.h"

(xffi:define-c-typedef (u-int16-t (:foreign-name "u_int16_t"))
                      (:unsigned :short))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_u_int32_t.h"

(xffi:define-c-typedef (u-int32-t (:foreign-name "u_int32_t"))
                      (:unsigned :int))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_u_int64_t.h"

(xffi:define-c-typedef (u-int64-t (:foreign-name "u_int64_t"))
                      (:unsigned :long-long))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/arm/types.h"

(xffi:define-c-typedef (register-t (:foreign-name "register_t")) int64-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_uintptr_t.h"

(xffi:define-c-typedef (uintptr-t (:foreign-name "uintptr_t"))
                      (:unsigned :long))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/arm/types.h"

(xffi:define-c-typedef (user-addr-t (:foreign-name "user_addr_t"))
                      u-int64-t)
(xffi:define-c-typedef (user-size-t (:foreign-name "user_size_t"))
                      u-int64-t)
(xffi:define-c-typedef (user-ssize-t (:foreign-name "user_ssize_t"))
                      int64-t)
(xffi:define-c-typedef (user-long-t (:foreign-name "user_long_t"))
                      int64-t)
(xffi:define-c-typedef (user-ulong-t (:foreign-name "user_ulong_t"))
                      u-int64-t)
(xffi:define-c-typedef (user-time-t (:foreign-name "user_time_t"))
                      int64-t)
(xffi:define-c-typedef (user-off-t (:foreign-name "user_off_t")) int64-t)
(xffi:define-c-typedef (syscall-arg-t (:foreign-name "syscall_arg_t"))
                      u-int64-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_intptr_t.h"

(xffi:define-c-typedef (intptr-t (:foreign-name "intptr_t"))
                      --darwin-intptr-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/_types/_intmax_t.h"

(xffi:define-c-typedef (intmax-t (:foreign-name "intmax_t")) :long)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/_types/_uintmax_t.h"

(xffi:define-c-typedef (uintmax-t (:foreign-name "uintmax_t"))
                      (:unsigned :long))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/libkern/arm/OSByteOrder.h"

(xffi:define-c-struct (-osunalignedu16
                      (:foreign-name "_OSUnalignedU16"))
                     (--val (:volatile uint16-t)))
(xffi:define-c-struct (-osunalignedu32
                      (:foreign-name "_OSUnalignedU32"))
                     (--val (:volatile uint32-t)))
(xffi:define-c-struct (-osunalignedu64
                      (:foreign-name "_OSUnalignedU64"))
                     (--val (:volatile uint64-t)))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_u_char.h"

(xffi:define-c-typedef (u-char (:foreign-name "u_char"))
                      (:unsigned :char))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_u_short.h"

(xffi:define-c-typedef (u-short (:foreign-name "u_short"))
                      (:unsigned :short))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_u_int.h"

(xffi:define-c-typedef (u-int (:foreign-name "u_int")) (:unsigned :int))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/types.h"

(xffi:define-c-typedef (u-long (:foreign-name "u_long"))
                      (:unsigned :long))
(xffi:define-c-typedef (ushort (:foreign-name "ushort"))
                      (:unsigned :short))
(xffi:define-c-typedef (uint (:foreign-name "uint")) (:unsigned :int))
(xffi:define-c-typedef (u-quad-t (:foreign-name "u_quad_t")) u-int64-t)
(xffi:define-c-typedef (quad-t (:foreign-name "quad_t")) int64-t)
(xffi:define-c-typedef (qaddr-t (:foreign-name "qaddr_t"))
                      (:pointer quad-t))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_caddr_t.h"

(xffi:define-c-typedef (caddr-t (:foreign-name "caddr_t"))
                      (:pointer :char))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/types.h"

(xffi:define-c-typedef (daddr-t (:foreign-name "daddr_t")) int32-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_dev_t.h"

(xffi:define-c-typedef (dev-t (:foreign-name "dev_t")) --darwin-dev-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/types.h"

(xffi:define-c-typedef (fixpt-t (:foreign-name "fixpt_t")) u-int32-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_blkcnt_t.h"

(xffi:define-c-typedef (blkcnt-t (:foreign-name "blkcnt_t"))
                      --darwin-blkcnt-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_blksize_t.h"

(xffi:define-c-typedef (blksize-t (:foreign-name "blksize_t"))
                      --darwin-blksize-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_gid_t.h"

(xffi:define-c-typedef (gid-t (:foreign-name "gid_t")) --darwin-gid-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_in_addr_t.h"

(xffi:define-c-typedef (in-addr-t (:foreign-name "in_addr_t"))
                      --uint32-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_in_port_t.h"

(xffi:define-c-typedef (in-port-t (:foreign-name "in_port_t"))
                      --uint16-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_ino_t.h"

(xffi:define-c-typedef (ino-t (:foreign-name "ino_t")) --darwin-ino-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_ino64_t.h"

(xffi:define-c-typedef (ino64-t (:foreign-name "ino64_t"))
                      --darwin-ino64-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_key_t.h"

(xffi:define-c-typedef (key-t (:foreign-name "key_t")) --int32-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_mode_t.h"

(xffi:define-c-typedef (mode-t (:foreign-name "mode_t")) --darwin-mode-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_nlink_t.h"

(xffi:define-c-typedef (nlink-t (:foreign-name "nlink_t")) --uint16-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_id_t.h"

(xffi:define-c-typedef (id-t (:foreign-name "id_t")) --darwin-id-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_pid_t.h"

(xffi:define-c-typedef (pid-t (:foreign-name "pid_t")) --darwin-pid-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_off_t.h"

(xffi:define-c-typedef (off-t (:foreign-name "off_t")) --darwin-off-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/types.h"

(xffi:define-c-typedef (segsz-t (:foreign-name "segsz_t")) int32-t)
(xffi:define-c-typedef (swblk-t (:foreign-name "swblk_t")) int32-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_uid_t.h"

(xffi:define-c-typedef (uid-t (:foreign-name "uid_t")) --darwin-uid-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_clock_t.h"

(xffi:define-c-typedef (clock-t (:foreign-name "clock_t"))
                      --darwin-clock-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_size_t.h"

(xffi:define-c-typedef (size-t (:foreign-name "size_t")) --darwin-size-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_ssize_t.h"

(xffi:define-c-typedef (ssize-t (:foreign-name "ssize_t"))
                      --darwin-ssize-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_time_t.h"

(xffi:define-c-typedef (time-t (:foreign-name "time_t")) --darwin-time-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_useconds_t.h"

(xffi:define-c-typedef (useconds-t (:foreign-name "useconds_t"))
                      --darwin-useconds-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_suseconds_t.h"

(xffi:define-c-typedef (suseconds-t (:foreign-name "suseconds_t"))
                      --darwin-suseconds-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_rsize_t.h"

(xffi:define-c-typedef (rsize-t (:foreign-name "rsize_t"))
                      --darwin-size-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_errno_t.h"

(xffi:define-c-typedef (errno-t (:foreign-name "errno_t")) :int)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_fd_def.h"

(xffi:define-c-struct (fd-set (:foreign-name "fd_set"))
                     (fds-bits (:c-array --int32-t 32)))
(xffi:define-c-typedef (fd-set (:foreign-name "fd_set"))
                      (:struct fd-set))
(xffi:define-foreign-function (--darwin-check-fd-set-overflow "__darwin_check_fd_set_overflow"
                                                             :source)
                             ((arg-1 :int)
                              (arg-2 (:pointer (:const :void)))
                              (arg-3 :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (--darwin-check-fd-set "__darwin_check_fd_set"
                                                    :source)
                             ((-a :int) (-b (:pointer (:const :void))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (--darwin-fd-isset "__darwin_fd_isset"
                                                :source)
                             ((-fd :int)
                              (-p
                               (:pointer (:const (:struct fd-set)))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (--darwin-fd-set "__darwin_fd_set"
                                              :source)
                             ((-fd :int)
                              (-p
                               (:const (:pointer (:struct fd-set)))))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (--darwin-fd-clr "__darwin_fd_clr"
                                              :source)
                             ((-fd :int)
                              (-p
                               (:const (:pointer (:struct fd-set)))))
                             :result-type
                             :void
                             :language
                             :ansi-c)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/types.h"

(xffi:define-c-typedef (fd-mask (:foreign-name "fd_mask")) --int32-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_pthread/_pthread_attr_t.h"

(xffi:define-c-typedef (pthread-attr-t (:foreign-name "pthread_attr_t"))
                      --darwin-pthread-attr-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_pthread/_pthread_cond_t.h"

(xffi:define-c-typedef (pthread-cond-t (:foreign-name "pthread_cond_t"))
                      --darwin-pthread-cond-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_pthread/_pthread_condattr_t.h"

(xffi:define-c-typedef (pthread-condattr-t
                       (:foreign-name "pthread_condattr_t"))
                      --darwin-pthread-condattr-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_pthread/_pthread_mutex_t.h"

(xffi:define-c-typedef (pthread-mutex-t
                       (:foreign-name "pthread_mutex_t"))
                      --darwin-pthread-mutex-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_pthread/_pthread_mutexattr_t.h"

(xffi:define-c-typedef (pthread-mutexattr-t
                       (:foreign-name "pthread_mutexattr_t"))
                      --darwin-pthread-mutexattr-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_pthread/_pthread_once_t.h"

(xffi:define-c-typedef (pthread-once-t (:foreign-name "pthread_once_t"))
                      --darwin-pthread-once-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_pthread/_pthread_rwlock_t.h"

(xffi:define-c-typedef (pthread-rwlock-t
                       (:foreign-name "pthread_rwlock_t"))
                      --darwin-pthread-rwlock-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_pthread/_pthread_rwlockattr_t.h"

(xffi:define-c-typedef (pthread-rwlockattr-t
                       (:foreign-name "pthread_rwlockattr_t"))
                      --darwin-pthread-rwlockattr-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_pthread/_pthread_t.h"

(xffi:define-c-typedef (pthread-t (:foreign-name "pthread_t"))
                      --darwin-pthread-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_pthread/_pthread_key_t.h"

(xffi:define-c-typedef (pthread-key-t (:foreign-name "pthread_key_t"))
                      --darwin-pthread-key-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_fsblkcnt_t.h"

(xffi:define-c-typedef (fsblkcnt-t (:foreign-name "fsblkcnt_t"))
                      --darwin-fsblkcnt-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_fsfilcnt_t.h"

(xffi:define-c-typedef (fsfilcnt-t (:foreign-name "fsfilcnt_t"))
                      --darwin-fsfilcnt-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_timespec.h"

(xffi:define-c-struct (timespec (:foreign-name "timespec"))
                     (tv-sec --darwin-time-t)
                     (tv-nsec :long))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_timeval.h"

(xffi:define-c-struct (timeval (:foreign-name "timeval"))
                     (tv-sec --darwin-time-t)
                     (tv-usec --darwin-suseconds-t))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_types/_timeval64.h"

(xffi:define-c-struct (timeval64 (:foreign-name "timeval64"))
                     (tv-sec --int64-t)
                     (tv-usec --int64-t))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/time.h"

(xffi:define-c-struct (itimerval (:foreign-name "itimerval"))
                     (it-interval (:struct timeval))
                     (it-value (:struct timeval)))
(xffi:define-c-struct (timezone (:foreign-name "timezone"))
                     (tz-minuteswest :int)
                     (tz-dsttime :int))
(xffi:define-c-struct (clockinfo (:foreign-name "clockinfo"))
                     (hz :int)
                     (tick :int)
                     (tickadj :int)
                     (stathz :int)
                     (profhz :int))

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/_types.h"

(xffi:define-c-typedef (--darwin-nl-item
                       (:foreign-name "__darwin_nl_item"))
                      :int)
(xffi:define-c-typedef (--darwin-wctrans-t
                       (:foreign-name "__darwin_wctrans_t"))
                      :int)
(xffi:define-c-typedef (--darwin-wctype-t
                       (:foreign-name "__darwin_wctype_t"))
                      --uint32-t)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/time.h"

(xffi:define-c-struct (tm (:foreign-name "tm"))
                     (tm-sec :int)
                     (tm-min :int)
                     (tm-hour :int)
                     (tm-mday :int)
                     (tm-mon :int)
                     (tm-year :int)
                     (tm-wday :int)
                     (tm-yday :int)
                     (tm-isdst :int)
                     (tm-gmtoff :long)
                     (tm-zone (:pointer :char)))
(xffi:define-foreign-variable (tzname "tzname" :source)
                             :type
                             (:c-array (:pointer :char))
                             :accessor
                             :address-of)
(xffi:define-foreign-variable (getdate-err "getdate_err" :source)
                             :type
                             :int)
(xffi:define-foreign-variable (timezone "timezone" :source) :type :long)
(xffi:define-foreign-variable (daylight "daylight" :source) :type :int)
(xffi:define-foreign-function (asctime "asctime" :source)
                             ((arg-1 (:pointer (:const (:struct tm)))))
                             :result-type
                             (:pointer :char)
                             :language
                             :ansi-c)
(xffi:define-foreign-function (clock "clock" :source)
                             nil
                             :result-type
                             clock-t
                             :language
                             :ansi-c)
(xffi:define-foreign-function (ctime "ctime" :source)
                             ((arg-1 (:pointer (:const time-t))))
                             :result-type
                             (:pointer :char)
                             :language
                             :ansi-c)
(xffi:define-foreign-function (difftime "difftime" :source)
                             ((arg-1 time-t) (arg-2 time-t))
                             :result-type
                             :double
                             :language
                             :ansi-c)
(xffi:define-foreign-function (getdate "getdate" :source)
                             ((arg-1 (:pointer (:const :char))))
                             :result-type
                             (:pointer (:struct tm))
                             :language
                             :ansi-c)
(xffi:define-foreign-function (gmtime "gmtime" :source)
                             ((arg-1 (:pointer (:const time-t))))
                             :result-type
                             (:pointer (:struct tm))
                             :language
                             :ansi-c)
(xffi:define-foreign-function (localtime "localtime" :source)
                             ((arg-1 (:pointer (:const time-t))))
                             :result-type
                             (:pointer (:struct tm))
                             :language
                             :ansi-c)
(xffi:define-foreign-function (mktime "mktime" :source)
                             ((arg-1 (:pointer (:struct tm))))
                             :result-type
                             time-t
                             :language
                             :ansi-c)
(xffi:define-foreign-function (strftime "strftime" :source)
                             ((arg-1 (:pointer :char))
                              (arg-2 size-t)
                              (arg-3 (:pointer (:const :char)))
                              (arg-4 (:pointer (:const (:struct tm)))))
                             :result-type
                             size-t
                             :language
                             :ansi-c)
(xffi:define-foreign-function (strptime "strptime" :source)
                             ((arg-1 (:pointer (:const :char)))
                              (arg-2 (:pointer (:const :char)))
                              (arg-3 (:pointer (:struct tm))))
                             :result-type
                             (:pointer :char)
                             :language
                             :ansi-c)
(xffi:define-foreign-function (%time "time" :source)
                             ((arg-1 (:pointer time-t)))
                             :result-type
                             time-t
                             :language
                             :ansi-c)
(xffi:define-foreign-function (tzset "tzset" :source)
                             nil
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (asctime-r "asctime_r" :source)
                             ((arg-1 (:pointer (:const (:struct tm))))
                              (arg-2 (:pointer :char)))
                             :result-type
                             (:pointer :char)
                             :language
                             :ansi-c)
(xffi:define-foreign-function (ctime-r "ctime_r" :source)
                             ((arg-1 (:pointer (:const time-t)))
                              (arg-2 (:pointer :char)))
                             :result-type
                             (:pointer :char)
                             :language
                             :ansi-c)
(xffi:define-foreign-function (gmtime-r "gmtime_r" :source)
                             ((arg-1 (:pointer (:const time-t)))
                              (arg-2 (:pointer (:struct tm))))
                             :result-type
                             (:pointer (:struct tm))
                             :language
                             :ansi-c)
(xffi:define-foreign-function (localtime-r "localtime_r" :source)
                             ((arg-1 (:pointer (:const time-t)))
                              (arg-2 (:pointer (:struct tm))))
                             :result-type
                             (:pointer (:struct tm))
                             :language
                             :ansi-c)
(xffi:define-foreign-function (posix2time "posix2time" :source)
                             ((arg-1 time-t))
                             :result-type
                             time-t
                             :language
                             :ansi-c)
(xffi:define-foreign-function (tzsetwall "tzsetwall" :source)
                             nil
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (time2posix "time2posix" :source)
                             ((arg-1 time-t))
                             :result-type
                             time-t
                             :language
                             :ansi-c)
(xffi:define-foreign-function (timelocal "timelocal" :source)
                             ((arg-1 (:const (:pointer (:struct tm)))))
                             :result-type
                             time-t
                             :language
                             :ansi-c)
(xffi:define-foreign-function (timegm "timegm" :source)
                             ((arg-1 (:const (:pointer (:struct tm)))))
                             :result-type
                             time-t
                             :language
                             :ansi-c)
(xffi:define-foreign-function (nanosleep "nanosleep" :source)
                             ((--rqtp
                               (:pointer (:const (:struct timespec))))
                              (--rmtp (:pointer (:struct timespec))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-c-typedef (clockid-t (:foreign-name "clockid_t"))
                      (:enum
                       (-clock-realtime 0)
                       (-clock-monotonic 6)
                       (-clock-monotonic-raw 4)
                       (-clock-monotonic-raw-approx 5)
                       (-clock-uptime-raw 8)
                       (-clock-uptime-raw-approx 9)
                       (-clock-process-cputime-id 12)
                       (-clock-thread-cputime-id 16)))
(xffi:define-foreign-function (clock-getres "clock_getres" :source)
                             ((--clock-id clockid-t)
                              (--res (:pointer (:struct timespec))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (clock-gettime "clock_gettime" :source)
                             ((--clock-id clockid-t)
                              (--tp (:pointer (:struct timespec))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (clock-gettime-nsec-np
                              "clock_gettime_nsec_np"
                              :source)
                             ((--clock-id clockid-t))
                             :result-type
                             --uint64-t
                             :language
                             :ansi-c)
(xffi:define-foreign-function (clock-settime "clock_settime" :source)
                             ((--clock-id clockid-t)
                              (--tp
                               (:pointer (:const (:struct timespec)))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (timespec-get "timespec_get" :source)
                             ((ts (:pointer (:struct timespec)))
                              (base :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/time.h"

(xffi:define-foreign-function (adjtime "adjtime" :source)
                             ((arg-1
                               (:pointer (:const (:struct timeval))))
                              (arg-2 (:pointer (:struct timeval))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (futimes "futimes" :source)
                             ((arg-1 :int)
                              (arg-2
                               (:pointer (:const (:struct timeval)))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (lutimes "lutimes" :source)
                             ((arg-1 (:pointer (:const :char)))
                              (arg-2
                               (:pointer (:const (:struct timeval)))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (settimeofday "settimeofday" :source)
                             ((arg-1
                               (:pointer (:const (:struct timeval))))
                              (arg-2
                               (:pointer (:const (:struct timezone)))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (getitimer "getitimer" :source)
                             ((arg-1 :int)
                              (arg-2 (:pointer (:struct itimerval))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (gettimeofday "gettimeofday" :source)
                             ((arg-1 (:pointer (:struct timeval)))
                              (arg-2 (:pointer :void)))
                             :result-type
                             :int
                             :language
                             :ansi-c)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/_select.h"

(xffi:define-foreign-function (select "select" :source)
                             ((arg-1 :int)
                              (arg-2 (:pointer fd-set))
                              (arg-3 (:pointer fd-set))
                              (arg-4 (:pointer fd-set))
                              (arg-5 (:pointer (:struct timeval))))
                             :result-type
                             :int
                             :language
                             :ansi-c)

;;; Derived from file : "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk/usr/include/sys/time.h"

(xffi:define-foreign-function (setitimer "setitimer" :source)
                             ((arg-1 :int)
                              (arg-2
                               (:pointer (:const (:struct itimerval))))
                              (arg-3 (:pointer (:struct itimerval))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (utimes "utimes" :source)
                             ((arg-1 (:pointer (:const :char)))
                              (arg-2
                               (:pointer (:const (:struct timeval)))))
                             :result-type
                             :int
                             :language
                             :ansi-c)

;;; ========================================================================================
;;; Derived from file : "/usr/local/include/libusb.h"

(xffi:define-c-enum (libusb-class-code
                    (:foreign-name "libusb_class_code"))
                   (libusb-class-per-interface 0)
                   (libusb-class-audio 1)
                   (libusb-class-comm 2)
                   (libusb-class-hid 3)
                   (libusb-class-physical 5)
                   (libusb-class-image 6)
                   (libusb-class-ptp 6)
                   (libusb-class-printer 7)
                   (libusb-class-mass-storage 8)
                   (libusb-class-hub 9)
                   (libusb-class-data 10)
                   (libusb-class-smart-card 11)
                   (libusb-class-content-security 13)
                   (libusb-class-video 14)
                   (libusb-class-personal-healthcare 15)
                   (libusb-class-diagnostic-device 220)
                   (libusb-class-wireless 224)
                   (libusb-class-miscellaneous 239)
                   (libusb-class-application 254)
                   (libusb-class-vendor-spec 255))
(xffi:define-c-enum (libusb-descriptor-type
                    (:foreign-name "libusb_descriptor_type"))
                   (libusb-dt-device 1)
                   (libusb-dt-config 2)
                   (libusb-dt-string 3)
                   (libusb-dt-interface 4)
                   (libusb-dt-endpoint 5)
                   (libusb-dt-bos 15)
                   (libusb-dt-device-capability 16)
                   (libusb-dt-hid 33)
                   (libusb-dt-report 34)
                   (libusb-dt-physical 35)
                   (libusb-dt-hub 41)
                   (libusb-dt-superspeed-hub 42)
                   (libusb-dt-ss-endpoint-companion 48))
(xffi:define-c-enum (libusb-endpoint-direction
                    (:foreign-name "libusb_endpoint_direction"))
                   (libusb-endpoint-out 0)
                   (libusb-endpoint-in 128))
(xffi:define-c-enum (libusb-endpoint-transfer-type
                    (:foreign-name "libusb_endpoint_transfer_type"))
                   (libusb-endpoint-transfer-type-control 0)
                   (libusb-endpoint-transfer-type-isochronous 1)
                   (libusb-endpoint-transfer-type-bulk 2)
                   (libusb-endpoint-transfer-type-interrupt 3))
(xffi:define-c-enum (libusb-standard-request
                    (:foreign-name "libusb_standard_request"))
                   (libusb-request-get-status 0)
                   (libusb-request-clear-feature 1)
                   (libusb-request-set-feature 3)
                   (libusb-request-set-address 5)
                   (libusb-request-get-descriptor 6)
                   (libusb-request-set-descriptor 7)
                   (libusb-request-get-configuration 8)
                   (libusb-request-set-configuration 9)
                   (libusb-request-get-interface 10)
                   (libusb-request-set-interface 11)
                   (libusb-request-synch-frame 12)
                   (libusb-request-set-sel 48)
                   (libusb-set-isoch-delay 49))
(xffi:define-c-enum (libusb-request-type
                    (:foreign-name "libusb_request_type"))
                   (libusb-request-type-standard 0)
                   (libusb-request-type-class 32)
                   (libusb-request-type-vendor 64)
                   (libusb-request-type-reserved 96))
(xffi:define-c-enum (libusb-request-recipient
                    (:foreign-name "libusb_request_recipient"))
                   (libusb-recipient-device 0)
                   (libusb-recipient-interface 1)
                   (libusb-recipient-endpoint 2)
                   (libusb-recipient-other 3))
(xffi:define-c-enum (libusb-iso-sync-type
                    (:foreign-name "libusb_iso_sync_type"))
                   (libusb-iso-sync-type-none 0)
                   (libusb-iso-sync-type-async 1)
                   (libusb-iso-sync-type-adaptive 2)
                   (libusb-iso-sync-type-sync 3))
(xffi:define-c-enum (libusb-iso-usage-type
                    (:foreign-name "libusb_iso_usage_type"))
                   (libusb-iso-usage-type-data 0)
                   (libusb-iso-usage-type-feedback 1)
                   (libusb-iso-usage-type-implicit 2))
(xffi:define-c-enum (libusb-supported-speed
                    (:foreign-name "libusb_supported_speed"))
                   (libusb-low-speed-operation 1)
                   (libusb-full-speed-operation 2)
                   (libusb-high-speed-operation 4)
                   (libusb-super-speed-operation 8))
(xffi:define-c-enum (libusb-usb-2-0-extension-attributes
                    (:foreign-name
                     "libusb_usb_2_0_extension_attributes"))
                   (libusb-bm-lpm-support 2))
(xffi:define-c-enum (libusb-ss-usb-device-capability-attributes
                    (:foreign-name
                     "libusb_ss_usb_device_capability_attributes"))
                   (libusb-bm-ltm-support 2))
(xffi:define-c-enum (libusb-bos-type (:foreign-name "libusb_bos_type"))
                   (libusb-bt-wireless-usb-device-capability 1)
                   (libusb-bt-usb-2-0-extension 2)
                   (libusb-bt-ss-usb-device-capability 3)
                   (libusb-bt-container-id 4))
(xffi:define-c-struct (libusb-device-descriptor
                      (:foreign-name "libusb_device_descriptor"))
                     (blength uint8-t)
                     (bdescriptortype uint8-t)
                     (bcdusb uint16-t)
                     (bdeviceclass uint8-t)
                     (bdevicesubclass uint8-t)
                     (bdeviceprotocol uint8-t)
                     (bmaxpacketsize0 uint8-t)
                     (idvendor uint16-t)
                     (idproduct uint16-t)
                     (bcddevice uint16-t)
                     (imanufacturer uint8-t)
                     (iproduct uint8-t)
                     (iserialnumber uint8-t)
                     (bnumconfigurations uint8-t))
(xffi:define-c-struct (libusb-endpoint-descriptor
                      (:foreign-name "libusb_endpoint_descriptor"))
                     (blength uint8-t)
                     (bdescriptortype uint8-t)
                     (bendpointaddress uint8-t)
                     (bmattributes uint8-t)
                     (wmaxpacketsize uint16-t)
                     (binterval uint8-t)
                     (brefresh uint8-t)
                     (bsynchaddress uint8-t)
                     (extra (:pointer (:const (:unsigned :char))))
                     (extra-length :int))
(xffi:define-c-struct (libusb-interface-descriptor
                      (:foreign-name "libusb_interface_descriptor"))
                     (blength uint8-t)
                     (bdescriptortype uint8-t)
                     (binterfacenumber uint8-t)
                     (balternatesetting uint8-t)
                     (bnumendpoints uint8-t)
                     (binterfaceclass uint8-t)
                     (binterfacesubclass uint8-t)
                     (binterfaceprotocol uint8-t)
                     (iinterface uint8-t)
                     (endpoint
                      (:pointer
                       (:const (:struct libusb-endpoint-descriptor))))
                     (extra (:pointer (:const (:unsigned :char))))
                     (extra-length :int))
(xffi:define-c-struct (libusb-interface
                      (:foreign-name "libusb_interface"))
                     (altsetting
                      (:pointer
                       (:const (:struct libusb-interface-descriptor))))
                     (num-altsetting :int))
(xffi:define-c-struct (libusb-config-descriptor
                      (:foreign-name "libusb_config_descriptor"))
                     (blength uint8-t)
                     (bdescriptortype uint8-t)
                     (wtotallength uint16-t)
                     (bnuminterfaces uint8-t)
                     (bconfigurationvalue uint8-t)
                     (iconfiguration uint8-t)
                     (bmattributes uint8-t)
                     (maxpower uint8-t)
                     (interface
                      (:pointer (:const (:struct libusb-interface))))
                     (extra (:pointer (:const (:unsigned :char))))
                     (extra-length :int))
(xffi:define-c-struct (libusb-ss-endpoint-companion-descriptor
                      (:foreign-name
                       "libusb_ss_endpoint_companion_descriptor"))
                     (blength uint8-t)
                     (bdescriptortype uint8-t)
                     (bmaxburst uint8-t)
                     (bmattributes uint8-t)
                     (wbytesperinterval uint16-t))
(xffi:define-c-struct (libusb-bos-dev-capability-descriptor
                      (:foreign-name
                       "libusb_bos_dev_capability_descriptor"))
                     (blength uint8-t)
                     (bdescriptortype uint8-t)
                     (bdevcapabilitytype uint8-t)
                     (dev-capability-data (:c-array uint8-t)))
(xffi:define-c-struct (libusb-bos-descriptor
                      (:foreign-name "libusb_bos_descriptor"))
                     (blength uint8-t)
                     (bdescriptortype uint8-t)
                     (wtotallength uint16-t)
                     (bnumdevicecaps uint8-t)
                     (dev-capability
                      (:c-array
                       (:pointer
                        (:struct
                         libusb-bos-dev-capability-descriptor)))))
(xffi:define-c-struct (libusb-usb-2-0-extension-descriptor
                      (:foreign-name
                       "libusb_usb_2_0_extension_descriptor"))
                     (blength uint8-t)
                     (bdescriptortype uint8-t)
                     (bdevcapabilitytype uint8-t)
                     (bmattributes uint32-t))
(xffi:define-c-struct (libusb-ss-usb-device-capability-descriptor
                      (:foreign-name
                       "libusb_ss_usb_device_capability_descriptor"))
                     (blength uint8-t)
                     (bdescriptortype uint8-t)
                     (bdevcapabilitytype uint8-t)
                     (bmattributes uint8-t)
                     (wspeedsupported uint16-t)
                     (bfunctionalitysupport uint8-t)
                     (bu1devexitlat uint8-t)
                     (bu2devexitlat uint16-t))
(xffi:define-c-struct (libusb-container-id-descriptor
                      (:foreign-name "libusb_container_id_descriptor"))
                     (blength uint8-t)
                     (bdescriptortype uint8-t)
                     (bdevcapabilitytype uint8-t)
                     (breserved uint8-t)
                     (containerid (:c-array uint8-t 16)))
(xffi:define-c-struct (libusb-control-setup
                      (:foreign-name "libusb_control_setup"))
                     (bmrequesttype uint8-t)
                     (brequest uint8-t)
                     (wvalue uint16-t)
                     (windex uint16-t)
                     (wlength uint16-t))
(xffi:define-c-struct (libusb-context
                      (:foreign-name "libusb_context")
                      (:forward-reference-p t)))
(xffi:define-c-struct (libusb-device
                      (:foreign-name "libusb_device")
                      (:forward-reference-p t)))
(xffi:define-c-struct (libusb-device-handle
                      (:foreign-name "libusb_device_handle")
                      (:forward-reference-p t)))
(xffi:define-c-struct (libusb-version (:foreign-name "libusb_version"))
                     (major (:const uint16-t))
                     (minor (:const uint16-t))
                     (micro (:const uint16-t))
                     (nano (:const uint16-t))
                     (rc (:pointer (:const :char)))
                     (describe (:pointer (:const :char))))
(xffi:define-c-typedef (libusb-context (:foreign-name "libusb_context"))
                      (:struct libusb-context))
(xffi:define-c-typedef (libusb-device (:foreign-name "libusb_device"))
                      (:struct libusb-device))
(xffi:define-c-typedef (libusb-device-handle
                       (:foreign-name "libusb_device_handle"))
                      (:struct libusb-device-handle))
(xffi:define-c-enum (libusb-speed (:foreign-name "libusb_speed"))
                   (libusb-speed-unknown 0)
                   (libusb-speed-low 1)
                   (libusb-speed-full 2)
                   (libusb-speed-high 3)
                   (libusb-speed-super 4)
                   (libusb-speed-super-plus 5))
(xffi:define-c-enum (libusb-error (:foreign-name "libusb_error"))
                   (libusb-success 0)
                   (libusb-error-io -1)
                   (libusb-error-invalid-param -2)
                   (libusb-error-access -3)
                   (libusb-error-no-device -4)
                   (libusb-error-not-found -5)
                   (libusb-error-busy -6)
                   (libusb-error-timeout -7)
                   (libusb-error-overflow -8)
                   (libusb-error-pipe -9)
                   (libusb-error-interrupted -10)
                   (libusb-error-no-mem -11)
                   (libusb-error-not-supported -12)
                   (libusb-error-other -99))
(xffi:define-c-enum (libusb-transfer-type
                    (:foreign-name "libusb_transfer_type"))
                   (libusb-transfer-type-control 0)
                   (libusb-transfer-type-isochronous 1)
                   (libusb-transfer-type-bulk 2)
                   (libusb-transfer-type-interrupt 3)
                   (libusb-transfer-type-bulk-stream 4))
(xffi:define-c-enum (libusb-transfer-status
                    (:foreign-name "libusb_transfer_status"))
                   (libusb-transfer-completed 0)
                   (libusb-transfer-error 1)
                   (libusb-transfer-timed-out 2)
                   (libusb-transfer-cancelled 3)
                   (libusb-transfer-stall 4)
                   (libusb-transfer-no-device 5)
                   (libusb-transfer-overflow 6))
(xffi:define-c-enum (libusb-transfer-flags
                    (:foreign-name "libusb_transfer_flags"))
                   (libusb-transfer-short-not-ok 1)
                   (libusb-transfer-free-buffer 2)
                   (libusb-transfer-free-transfer 4)
                   (libusb-transfer-add-zero-packet 8))
(xffi:define-c-struct (libusb-iso-packet-descriptor
                      (:foreign-name "libusb_iso_packet_descriptor"))
                     (length (:unsigned :int))
                     (actual-length (:unsigned :int))
                     (status (:enum libusb-transfer-status)))
(xffi:define-c-struct (libusb-transfer
                      (:foreign-name "libusb_transfer")
                      (:forward-reference-p t)))
(xffi:define-c-typedef (libusb-transfer-cb-fn
                       (:foreign-name "libusb_transfer_cb_fn"))
                      (:pointer
                       (:function
                        ((:pointer (:struct libusb-transfer)))
                        :void)))
(xffi:define-c-struct (libusb-transfer
                      (:foreign-name "libusb_transfer"))
                     (dev-handle (:pointer libusb-device-handle))
                     (flags uint8-t)
                     (endpoint (:unsigned :char))
                     (type (:unsigned :char))
                     (timeout (:unsigned :int))
                     (status (:enum libusb-transfer-status))
                     (length :int)
                     (actual-length :int)
                     (callback libusb-transfer-cb-fn)
                     (user-data (:pointer :void))
                     (buffer (:pointer (:unsigned :char)))
                     (num-iso-packets :int)
                     (iso-packet-desc
                      (:c-array
                       (:struct libusb-iso-packet-descriptor))))
(xffi:define-c-enum (libusb-capability
                    (:foreign-name "libusb_capability"))
                   (libusb-cap-has-capability 0)
                   (libusb-cap-has-hotplug 1)
                   (libusb-cap-has-hid-access 256)
                   (libusb-cap-supports-detach-kernel-driver 257))
(xffi:define-c-enum (libusb-log-level
                    (:foreign-name "libusb_log_level"))
                   (libusb-log-level-none 0)
                   (libusb-log-level-error 1)
                   (libusb-log-level-warning 2)
                   (libusb-log-level-info 3)
                   (libusb-log-level-debug 4))
(xffi:define-c-enum (libusb-log-cb-mode
                    (:foreign-name "libusb_log_cb_mode"))
                   (libusb-log-cb-global 1)
                   (libusb-log-cb-context 2))
(xffi:define-c-typedef (libusb-log-cb (:foreign-name "libusb_log_cb"))
                      (:pointer
                       (:function
                        ((:pointer libusb-context)
                         (:enum libusb-log-level)
                         (:pointer (:const :char)))
                        :void)))
(xffi:define-foreign-function (libusb-init "libusb_init" :source)
                             ((ctx
                               (:pointer (:pointer libusb-context))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-exit "libusb_exit" :source)
                             ((ctx (:pointer libusb-context)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-set-debug "libusb_set_debug"
                                               :source)
                             ((ctx (:pointer libusb-context))
                              (level :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-set-log-cb "libusb_set_log_cb"
                                                :source)
                             ((ctx (:pointer libusb-context))
                              (cb libusb-log-cb)
                              (mode :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-version "libusb_get_version"
                                                 :source)
                             nil
                             :result-type
                             (:pointer
                              (:const (:struct libusb-version)))
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-has-capability "libusb_has_capability"
                                                    :source)
                             ((capability uint32-t))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-error-name "libusb_error_name"
                                                :source)
                             ((errcode :int))
                             :result-type
                             (:pointer (:const :char))
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-setlocale "libusb_setlocale"
                                               :source)
                             ((locale (:pointer (:const :char))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-strerror "libusb_strerror"
                                              :source)
                             ((errcode :int))
                             :result-type
                             (:pointer (:const :char))
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-device-list "libusb_get_device_list"
                                                     :source)
                             ((ctx (:pointer libusb-context))
                              (list (:pointer
                                     (:pointer
                                      (:pointer libusb-device)))))
                             :result-type
                             ssize-t
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-free-device-list "libusb_free_device_list"
                                                      :source)
                             ((list (:pointer
                                     (:pointer libusb-device)))
                              (unref-devices :int))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-ref-device "libusb_ref_device"
                                                :source)
                             ((dev (:pointer libusb-device)))
                             :result-type
                             (:pointer libusb-device)
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-unref-device "libusb_unref_device"
                                                  :source)
                             ((dev (:pointer libusb-device)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-configuration "libusb_get_configuration"
                                                       :source)
                             ((dev (:pointer libusb-device-handle))
                              (config (:pointer :int)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-device-descriptor "libusb_get_device_descriptor"
                                                           :source)
                             ((dev (:pointer libusb-device))
                              (desc
                               (:pointer
                                (:struct libusb-device-descriptor))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-active-config-descriptor "libusb_get_active_config_descriptor"
                                                                  :source)
                             ((dev (:pointer libusb-device))
                              (config
                               (:pointer
                                (:pointer
                                 (:struct libusb-config-descriptor)))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-config-descriptor "libusb_get_config_descriptor"
                                                           :source)
                             ((dev (:pointer libusb-device))
                              (config-index uint8-t)
                              (config
                               (:pointer
                                (:pointer
                                 (:struct libusb-config-descriptor)))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-config-descriptor-by-value "libusb_get_config_descriptor_by_value"
                                                                    :source)
                             ((dev (:pointer libusb-device))
                              (bconfigurationvalue uint8-t)
                              (config
                               (:pointer
                                (:pointer
                                 (:struct libusb-config-descriptor)))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-free-config-descriptor "libusb_free_config_descriptor"
                                                            :source)
                             ((config
                               (:pointer
                                (:struct libusb-config-descriptor))))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-ss-endpoint-companion-descriptor "libusb_get_ss_endpoint_companion_descriptor"
                                                                          :source)
                             ((ctx (:pointer libusb-context))
                              (endpoint
                               (:pointer
                                (:const
                                 (:struct
                                  libusb-endpoint-descriptor))))
                              (ep-comp
                               (:pointer
                                (:pointer
                                 (:struct
                                  libusb-ss-endpoint-companion-descriptor)))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-free-ss-endpoint-companion-descriptor "libusb_free_ss_endpoint_companion_descriptor"
                                                                           :source)
                             ((ep-comp
                               (:pointer
                                (:struct
                                 libusb-ss-endpoint-companion-descriptor))))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-bos-descriptor "libusb_get_bos_descriptor"
                                                        :source)
                             ((dev-handle
                               (:pointer libusb-device-handle))
                              (bos
                               (:pointer
                                (:pointer
                                 (:struct libusb-bos-descriptor)))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-free-bos-descriptor "libusb_free_bos_descriptor"
                                                         :source)
                             ((bos
                               (:pointer
                                (:struct libusb-bos-descriptor))))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-usb-2-0-extension-descriptor "libusb_get_usb_2_0_extension_descriptor"
                                                                      :source)
                             ((ctx (:pointer libusb-context))
                              (dev-cap
                               (:pointer
                                (:struct
                                 libusb-bos-dev-capability-descriptor)))
                              (usb-2-0-extension
                               (:pointer
                                (:pointer
                                 (:struct
                                  libusb-usb-2-0-extension-descriptor)))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-free-usb-2-0-extension-descriptor "libusb_free_usb_2_0_extension_descriptor"
                                                                       :source)
                             ((usb-2-0-extension
                               (:pointer
                                (:struct
                                 libusb-usb-2-0-extension-descriptor))))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-ss-usb-device-capability-descriptor "libusb_get_ss_usb_device_capability_descriptor"
                                                                             :source)
                             ((ctx (:pointer libusb-context))
                              (dev-cap
                               (:pointer
                                (:struct
                                 libusb-bos-dev-capability-descriptor)))
                              (ss-usb-device-cap
                               (:pointer
                                (:pointer
                                 (:struct
                                  libusb-ss-usb-device-capability-descriptor)))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-free-ss-usb-device-capability-descriptor "libusb_free_ss_usb_device_capability_descriptor"
                                                                              :source)
                             ((ss-usb-device-cap
                               (:pointer
                                (:struct
                                 libusb-ss-usb-device-capability-descriptor))))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-container-id-descriptor "libusb_get_container_id_descriptor"
                                                                 :source)
                             ((ctx (:pointer libusb-context))
                              (dev-cap
                               (:pointer
                                (:struct
                                 libusb-bos-dev-capability-descriptor)))
                              (container-id
                               (:pointer
                                (:pointer
                                 (:struct
                                  libusb-container-id-descriptor)))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-free-container-id-descriptor "libusb_free_container_id_descriptor"
                                                                  :source)
                             ((container-id
                               (:pointer
                                (:struct
                                 libusb-container-id-descriptor))))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-bus-number "libusb_get_bus_number"
                                                    :source)
                             ((dev (:pointer libusb-device)))
                             :result-type
                             uint8-t
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-port-number "libusb_get_port_number"
                                                     :source)
                             ((dev (:pointer libusb-device)))
                             :result-type
                             uint8-t
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-port-numbers "libusb_get_port_numbers"
                                                      :source)
                             ((dev (:pointer libusb-device))
                              (port-numbers (:pointer uint8-t))
                              (port-numbers-len :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-port-path "libusb_get_port_path"
                                                   :source)
                             ((ctx (:pointer libusb-context))
                              (dev (:pointer libusb-device))
                              (path (:pointer uint8-t))
                              (path-length uint8-t))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-parent "libusb_get_parent"
                                                :source)
                             ((dev (:pointer libusb-device)))
                             :result-type
                             (:pointer libusb-device)
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-device-address "libusb_get_device_address"
                                                        :source)
                             ((dev (:pointer libusb-device)))
                             :result-type
                             uint8-t
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-device-speed "libusb_get_device_speed"
                                                      :source)
                             ((dev (:pointer libusb-device)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-max-packet-size "libusb_get_max_packet_size"
                                                         :source)
                             ((dev (:pointer libusb-device))
                              (endpoint (:unsigned :char)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-max-iso-packet-size "libusb_get_max_iso_packet_size"
                                                             :source)
                             ((dev (:pointer libusb-device))
                              (endpoint (:unsigned :char)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-wrap-sys-device "libusb_wrap_sys_device"
                                                     :source)
                             ((ctx (:pointer libusb-context))
                              (sys-dev intptr-t)
                              (dev-handle
                               (:pointer
                                (:pointer libusb-device-handle))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-open "libusb_open" :source)
                             ((dev (:pointer libusb-device))
                              (dev-handle
                               (:pointer
                                (:pointer libusb-device-handle))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-close "libusb_close" :source)
                             ((dev-handle
                               (:pointer libusb-device-handle)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-device "libusb_get_device"
                                                :source)
                             ((dev-handle
                               (:pointer libusb-device-handle)))
                             :result-type
                             (:pointer libusb-device)
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-set-configuration "libusb_set_configuration"
                                                       :source)
                             ((dev-handle
                               (:pointer libusb-device-handle))
                              (configuration :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-claim-interface "libusb_claim_interface"
                                                     :source)
                             ((dev-handle
                               (:pointer libusb-device-handle))
                              (interface-number :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-release-interface "libusb_release_interface"
                                                       :source)
                             ((dev-handle
                               (:pointer libusb-device-handle))
                              (interface-number :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-open-device-with-vid-pid "libusb_open_device_with_vid_pid"
                                                              :source)
                             ((ctx (:pointer libusb-context))
                              (vendor-id uint16-t)
                              (product-id uint16-t))
                             :result-type
                             (:pointer libusb-device-handle)
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-set-interface-alt-setting "libusb_set_interface_alt_setting"
                                                               :source)
                             ((dev-handle
                               (:pointer libusb-device-handle))
                              (interface-number :int)
                              (alternate-setting :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-clear-halt "libusb_clear_halt"
                                                :source)
                             ((dev-handle
                               (:pointer libusb-device-handle))
                              (endpoint (:unsigned :char)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-reset-device "libusb_reset_device"
                                                  :source)
                             ((dev-handle
                               (:pointer libusb-device-handle)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-alloc-streams "libusb_alloc_streams"
                                                   :source)
                             ((dev-handle
                               (:pointer libusb-device-handle))
                              (num-streams uint32-t)
                              (endpoints (:pointer (:unsigned :char)))
                              (num-endpoints :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-free-streams "libusb_free_streams"
                                                  :source)
                             ((dev-handle
                               (:pointer libusb-device-handle))
                              (endpoints (:pointer (:unsigned :char)))
                              (num-endpoints :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-dev-mem-alloc "libusb_dev_mem_alloc"
                                                   :source)
                             ((dev-handle
                               (:pointer libusb-device-handle))
                              (length size-t))
                             :result-type
                             (:pointer (:unsigned :char))
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-dev-mem-free "libusb_dev_mem_free"
                                                  :source)
                             ((dev-handle
                               (:pointer libusb-device-handle))
                              (buffer (:pointer (:unsigned :char)))
                              (length size-t))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-kernel-driver-active "libusb_kernel_driver_active"
                                                          :source)
                             ((dev-handle
                               (:pointer libusb-device-handle))
                              (interface-number :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-detach-kernel-driver "libusb_detach_kernel_driver"
                                                          :source)
                             ((dev-handle
                               (:pointer libusb-device-handle))
                              (interface-number :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-attach-kernel-driver "libusb_attach_kernel_driver"
                                                          :source)
                             ((dev-handle
                               (:pointer libusb-device-handle))
                              (interface-number :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-set-auto-detach-kernel-driver "libusb_set_auto_detach_kernel_driver"
                                                                   :source)
                             ((dev-handle
                               (:pointer libusb-device-handle))
                              (enable :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-alloc-transfer "libusb_alloc_transfer"
                                                    :source)
                             ((iso-packets :int))
                             :result-type
                             (:pointer (:struct libusb-transfer))
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-submit-transfer "libusb_submit_transfer"
                                                     :source)
                             ((transfer
                               (:pointer (:struct libusb-transfer))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-cancel-transfer "libusb_cancel_transfer"
                                                     :source)
                             ((transfer
                               (:pointer (:struct libusb-transfer))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-free-transfer "libusb_free_transfer"
                                                   :source)
                             ((transfer
                               (:pointer (:struct libusb-transfer))))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-transfer-set-stream-id "libusb_transfer_set_stream_id"
                                                            :source)
                             ((transfer
                               (:pointer (:struct libusb-transfer)))
                              (stream-id uint32-t))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-transfer-get-stream-id "libusb_transfer_get_stream_id"
                                                            :source)
                             ((transfer
                               (:pointer (:struct libusb-transfer))))
                             :result-type
                             uint32-t
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-control-transfer "libusb_control_transfer"
                                                      :source)
                             ((dev-handle
                               (:pointer libusb-device-handle))
                              (request-type uint8-t)
                              (brequest uint8-t)
                              (wvalue uint16-t)
                              (windex uint16-t)
                              (data (:pointer (:unsigned :char)))
                              (wlength uint16-t)
                              (timeout (:unsigned :int)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-bulk-transfer "libusb_bulk_transfer"
                                                   :source)
                             ((dev-handle
                               (:pointer libusb-device-handle))
                              (endpoint (:unsigned :char))
                              (data (:pointer (:unsigned :char)))
                              (length :int)
                              (actual-length (:pointer :int))
                              (timeout (:unsigned :int)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-interrupt-transfer "libusb_interrupt_transfer"
                                                        :source)
                             ((dev-handle
                               (:pointer libusb-device-handle))
                              (endpoint (:unsigned :char))
                              (data (:pointer (:unsigned :char)))
                              (length :int)
                              (actual-length (:pointer :int))
                              (timeout (:unsigned :int)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-string-descriptor-ascii "libusb_get_string_descriptor_ascii"
                                                                 :source)
                             ((dev-handle
                               (:pointer libusb-device-handle))
                              (desc-index uint8-t)
                              (data (:pointer (:unsigned :char)))
                              (length :int))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-try-lock-events "libusb_try_lock_events"
                                                     :source)
                             ((ctx (:pointer libusb-context)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-lock-events "libusb_lock_events"
                                                 :source)
                             ((ctx (:pointer libusb-context)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-unlock-events "libusb_unlock_events"
                                                   :source)
                             ((ctx (:pointer libusb-context)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-event-handling-ok "libusb_event_handling_ok"
                                                       :source)
                             ((ctx (:pointer libusb-context)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-event-handler-active "libusb_event_handler_active"
                                                          :source)
                             ((ctx (:pointer libusb-context)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-interrupt-event-handler "libusb_interrupt_event_handler"
                                                             :source)
                             ((ctx (:pointer libusb-context)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-lock-event-waiters "libusb_lock_event_waiters"
                                                        :source)
                             ((ctx (:pointer libusb-context)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-unlock-event-waiters "libusb_unlock_event_waiters"
                                                          :source)
                             ((ctx (:pointer libusb-context)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-wait-for-event "libusb_wait_for_event"
                                                    :source)
                             ((ctx (:pointer libusb-context))
                              (tv (:pointer (:struct timeval))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-handle-events-timeout "libusb_handle_events_timeout"
                                                           :source)
                             ((ctx (:pointer libusb-context))
                              (tv (:pointer (:struct timeval))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-handle-events-timeout-completed "libusb_handle_events_timeout_completed"
                                                                     :source)
                             ((ctx (:pointer libusb-context))
                              (tv (:pointer (:struct timeval)))
                              (completed (:pointer :int)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-handle-events "libusb_handle_events"
                                                   :source)
                             ((ctx (:pointer libusb-context)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-handle-events-completed "libusb_handle_events_completed"
                                                             :source)
                             ((ctx (:pointer libusb-context))
                              (completed (:pointer :int)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-handle-events-locked "libusb_handle_events_locked"
                                                          :source)
                             ((ctx (:pointer libusb-context))
                              (tv (:pointer (:struct timeval))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-pollfds-handle-timeouts "libusb_pollfds_handle_timeouts"
                                                             :source)
                             ((ctx (:pointer libusb-context)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-get-next-timeout "libusb_get_next_timeout"
                                                      :source)
                             ((ctx (:pointer libusb-context))
                              (tv (:pointer (:struct timeval))))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-c-struct (libusb-pollfd (:foreign-name "libusb_pollfd"))
                     (fd :int)
                     (events :short))
(xffi:define-c-typedef (libusb-pollfd-added-cb
                       (:foreign-name "libusb_pollfd_added_cb"))
                      (:pointer
                       (:function
                        (:int :short (:pointer :void))
                        :void)))
(xffi:define-c-typedef (libusb-pollfd-removed-cb
                       (:foreign-name "libusb_pollfd_removed_cb"))
                      (:pointer
                       (:function (:int (:pointer :void)) :void)))
(xffi:define-foreign-function (libusb-get-pollfds "libusb_get_pollfds"
                                                 :source)
                             ((ctx (:pointer libusb-context)))
                             :result-type
                             (:pointer
                              (:pointer
                               (:const (:struct libusb-pollfd))))
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-free-pollfds "libusb_free_pollfds"
                                                  :source)
                             ((pollfds
                               (:pointer
                                (:pointer
                                 (:const (:struct libusb-pollfd))))))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-set-pollfd-notifiers "libusb_set_pollfd_notifiers"
                                                          :source)
                             ((ctx (:pointer libusb-context))
                              (added-cb libusb-pollfd-added-cb)
                              (removed-cb libusb-pollfd-removed-cb)
                              (user-data (:pointer :void)))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-c-typedef (libusb-hotplug-callback-handle
                       (:foreign-name
                        "libusb_hotplug_callback_handle"))
                      :int)
(xffi:define-c-typedef (libusb-hotplug-event
                       (:foreign-name "libusb_hotplug_event"))
                      (:enum
                       (libusb-hotplug-event-device-arrived 1)
                       (libusb-hotplug-event-device-left 2)))
(xffi:define-c-typedef (libusb-hotplug-flag
                       (:foreign-name "libusb_hotplug_flag"))
                      (:enum (libusb-hotplug-enumerate 1)))
(xffi:define-c-typedef (libusb-hotplug-callback-fn
                       (:foreign-name "libusb_hotplug_callback_fn"))
                      (:pointer
                       (:function
                        ((:pointer libusb-context)
                         (:pointer libusb-device)
                         libusb-hotplug-event
                         (:pointer :void))
                        :int)))
(xffi:define-foreign-function (libusb-hotplug-register-callback "libusb_hotplug_register_callback"
                                                               :source)
                             ((ctx (:pointer libusb-context))
                              (events :int)
                              (flags :int)
                              (vendor-id :int)
                              (product-id :int)
                              (dev-class :int)
                              (cb-fn libusb-hotplug-callback-fn)
                              (user-data (:pointer :void))
                              (callback-handle
                               (:pointer
                                libusb-hotplug-callback-handle)))
                             :result-type
                             :int
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-hotplug-deregister-callback "libusb_hotplug_deregister_callback"
                                                                 :source)
                             ((ctx (:pointer libusb-context))
                              (callback-handle
                               libusb-hotplug-callback-handle))
                             :result-type
                             :void
                             :language
                             :ansi-c)
(xffi:define-foreign-function (libusb-hotplug-get-user-data "libusb_hotplug_get_user_data"
                                                           :source)
                             ((ctx (:pointer libusb-context))
                              (callback-handle
                               libusb-hotplug-callback-handle))
                             :result-type
                             (:pointer :void)
                             :language
                             :ansi-c)
(xffi:define-c-enum (libusb-option (:foreign-name "libusb_option"))
                   (libusb-option-log-level 0)
                   (libusb-option-use-usbdk 1)
                   (libusb-option-no-device-discovery 2)
                   (libusb-option-max 3))
(xffi:define-foreign-function (libusb-set-option "libusb_set_option"
                                                :source)
                             ((ctx (:pointer libusb-context))
                              (option (:enum libusb-option)))
                             :result-type
                             :int
                             :language
                             :ansi-c)

