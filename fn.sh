# From https://www.superuser.com/questions/79822/how-to-swap-the-fn-use-of-function-keys-on-al-apple-keyboard-in-linux
# 0 : Fn keys disabled
# 1 : Fn keys pressed by default
# 2 : Fn keys released by default

echo 1 > /sys/module/hid_apple/parameters/fnmode
