system 'make'
(1..49).each do |i|
    testfile = "test#{i.to_s}.tig"
    printf "\033[1;36;40m" + testfile + "\033[0m" + ': ' + "\033[0;33;40m\n"
    system './a.out ../testcases/' + testfile
    printf "\033[0m"
end
