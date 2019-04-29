system 'make'
(1..48).each do |i|
    system './a.out ../testcases/test' + i.to_s + '.tig' +
                ' > ../testoutput/out' + i.to_s + '.txt'
end

print "A syntax error testcase: "
system './a.out ../testcases/test49.tig'
