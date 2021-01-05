(ns aoc2020.day-02
  (:require [clojure.string :as str]))

(def input "
1-4 n: nnnnn
5-7 z: qhcgzzz
7-11 m: mmmmmmsmmmmm
5-8 d: ldddtdgnzddddwl
16-18 q: qsqqqqqqqqqpqqqlqhq
5-7 s: bwkbdlwns
14-17 v: vvvvvvvvvvvvvpvvxv
4-5 v: mvkvvn
2-5 h: lcwghhkpkxvzkvrmxrv
2-9 m: kmdvdlvxmhgsmlzp
3-14 t: twtblftnmmxttsdcm
7-19 f: cmshsffhcvdvzlgfbhnf
2-10 d: zvldxhnlqzp
5-9 w: wwwwwwwwwwww
5-6 c: dcxnwl
3-11 x: bjxxtkkxxtxxxpxcwb
10-13 m: bcztmmvpjvtxmtnlvc
8-11 x: xxrbhxskxxf
3-9 c: cvkzvcgccnc
9-20 j: jgxmzcwtqqsgqrgbrhpf
3-7 m: mvmmgvm
2-4 j: lrgbqjjfc
7-8 l: dllllxlllll
3-14 j: bgxgnmwjvghnfn
4-6 m: dzmmtmzm
8-16 r: rrrrrrrrrrlrrrrz
7-11 z: zlzzzzxzzzmzzn
3-11 k: wsvtblcbkkrfskkkkkz
5-6 v: vvvvvc
6-10 x: xxxxzxxxxkxxm
6-7 n: nvnnnjn
14-17 l: lllhllllnprkllpjlls
9-11 v: lmhrcwgvlzgjv
12-13 t: tttttttvttttttt
14-20 q: qnqqqqqcqqsqqqqpkqgq
15-16 w: wwwwwwpwwwwwwwww
4-10 f: fffffqsfsffffff
1-4 c: tcch
3-4 j: jvpl
5-12 q: qqrqqqqqvqqqqqqjq
2-4 l: llglfdllnxhdz
14-15 r: rrrrrrrrrrrbrrnrr
3-5 l: tzllllhrdhd
1-3 r: rrrr
8-9 m: mmmmmkmmm
15-17 k: kkrkkkkckkkkkkhkvkt
4-5 s: pxmsks
3-5 q: dxqqwvz
1-8 g: mgdggghrgngggcr
1-3 m: mwmcmrmtmmmmmmmmm
15-18 w: rjczjghkbqtgxswvpww
4-5 x: bsnxd
4-11 s: sssssssssssssss
7-10 g: gggggggggggglrl
12-13 t: ttttctttlttttttqw
8-11 m: mmmmmmmjmmdmmmmm
1-6 f: wnfffcqfffvfff
2-3 v: svvnsnq
4-8 j: gmkggjbrngwmrjpj
12-13 g: ggjmlgggxggqrgggg
2-5 t: jbztktqt
2-6 n: snvlknrn
10-11 r: rrrrrxrrrjjrhrrrr
7-9 l: lhgnlnklllbltvcr
8-13 r: vtttrkgrrwzfrvgngbrg
2-6 m: smlcmcmml
2-3 k: ktkkkk
4-8 x: xzxrcdjpcpgggwscfcb
4-6 t: tttwtt
1-4 b: bgbb
2-5 q: qqrfbzzbmtzvjxx
15-17 d: ddddddddddddddddw
5-6 j: kjwrtjjjnjcgldjn
4-5 v: vjvvh
11-12 z: zzzzgzzzzszz
1-2 t: tvbttt
4-7 k: xkkkkkwcksd
2-11 c: cbccccccccc
4-14 q: xcvqqmlqqcsvqq
11-12 f: ffffffffffxf
17-19 q: qqqqqqqqqqqqqqqqtqq
5-9 m: wfsjmjjlmn
6-13 n: nnnnnnnnnnnnnn
2-6 h: kcsrffwvnm
9-10 m: nvmrkkjhmt
2-5 w: xwwwq
4-8 b: cbfbxbtvdbbjzk
2-7 t: tttttxp
3-6 z: zzgzzbz
13-16 z: zzzzxzzzzzzzczzpz
5-15 l: llllllllllllllll
1-3 b: wbbb
3-8 m: mmnmmmmhm
11-12 t: tttttttttttl
6-10 k: kkkkkjkxkh
2-7 l: lmpblxllv
7-14 n: kmrsbkmnxnknnfnnsr
11-13 c: ccccshtsncmcs
8-10 f: kqmkkshfpm
1-3 t: tfdtlkmtnt
2-4 n: nnnnnn
7-19 c: crtqrtccctqckcthntc
4-5 k: kkzmqxkkbhsxkj
12-13 r: gzrrpvsrnbffrj
2-3 g: gcpg
5-7 m: msmwzmzlmrgffmjmj
8-9 r: rrrrrrrfr
7-12 d: dddddddddddd
2-3 z: vwvz
2-10 h: rhhhhhkksmhqp
14-15 x: xxxxxxxxxxxcxrxxx
6-8 g: pggsgxgxgmf
3-8 s: ssgnsssgsss
2-8 p: nhxncmzcdrfmrzmph
11-13 l: llllllllllnllll
5-15 v: lvvgrvgbcvvvdnhvvkr
10-20 l: qflrdpftwppjhsllkrvp
4-5 v: vvvxf
13-14 p: pppppppppppppp
4-7 f: qzxfgffncl
3-5 d: ddddd
12-18 k: kkhnvlfkkszqkgkgbx
12-15 g: pdtgqxngllvgjgg
6-7 p: pprppvrpp
1-4 b: fbbkb
4-7 f: rmjffqtdbf
2-8 j: jdjjjjjdjjj
15-16 t: ttttttttttttttttt
18-20 c: kftmnfrwjfccdfhdkcpc
1-3 r: rjvrrr
9-11 v: gvvvvvsjvvvxvvvvb
8-10 p: pkpppnpppfxp
9-10 t: xtthzvcktttwnplp
7-15 f: bcpwvxfsthfgpqf
4-7 s: sssvssnsss
4-15 h: tpchvgtmqqnvghhhn
4-15 r: nfscqbrzfhwfnlr
5-6 w: wwwwxlwww
6-7 q: zqhqqzzqq
5-9 m: cvqdhnlkzcrkpfjmzmck
3-4 g: ghqx
3-4 g: gzgg
11-15 w: wwwwwwwwwwswwwwwwww
5-7 z: djzrhhxhdfkm
6-7 m: nmmktfscmm
1-3 l: ljllrtllsf
8-11 z: zhzrzztqzzzlzhzhztpz
2-4 r: nrjmrlpr
1-7 w: nwwwwww
1-5 s: rfmch
1-2 r: rsrr
1-8 n: bnnnnnnwn
3-8 t: tdtdqwcpjdrkn
7-9 d: smcfcnrrdqvdsbcf
5-7 m: mmmmqmmm
1-10 h: thhhchjhhccchhdh
13-15 p: pppppqsptprpkpgpp
9-14 s: sspsxznskpsssqnw
3-10 z: zzzzzzzzzzzzzzz
2-4 t: fdptd
4-5 j: tjjrj
12-14 g: gggggggggggfgg
3-6 w: ncwcgwwg
5-7 n: nnnnnnnndg
2-10 v: zbvvvxvvvcb
16-18 r: jwjnrrhrztcgrhsrtr
2-3 v: vhcxv
9-10 z: zzzzzbzzzz
5-13 r: frprrzrrjjrrrn
5-6 n: njnnnnrnnzt
2-3 k: xmdkqzlbs
11-14 b: bbbbbbbbbbbbbb
1-2 w: wwwwwww
2-6 p: gppzgm
2-3 c: ncczxxmkdkjcp
1-6 d: vdvdzsmnvdfmddpqwmds
14-19 t: ttnttjjtvcbmtttgjtq
5-6 n: nnqckzm
8-14 h: rkpdfwnhhfjcvhl
3-9 g: sggsvgjkc
6-10 z: tzmdkwzjpzs
10-14 x: xxxxxxxxxlxxxnxxx
14-15 z: zzqzzzzzzzzsmgxzz
4-10 b: gbbbblmbckmlgbbwcbbb
1-2 v: lvwkv
3-14 v: qfvbdrpkrtvzcv
9-15 q: khqqbcqqbqvqqnmqqqq
16-18 m: tmfcmmpnmmtmbqcmtm
9-11 r: wrrrrrrrsrktrqr
1-2 k: kzgcckk
4-5 b: bbbbbb
6-7 l: lllllll
2-3 l: lrnlllql
7-9 w: wwwwhdwwlnwnwv
6-7 s: qkspdlfnsntxwpgb
7-11 c: mmrcckccccc
1-4 d: wvpdnh
3-8 z: zslnzzrszzp
4-5 t: ttptt
13-16 t: tttttrtttttttttt
3-4 t: tjtt
3-11 d: tbdwzwxbckdkdqlq
7-8 k: vkfqqdkk
6-8 q: dtqqqqqsq
1-12 d: bdddddddxdddd
1-15 s: sssnsnrsczswsss
7-17 c: cvnmxdxdkhxwhhmdhvd
4-8 z: qzvglftz
4-5 g: gggjg
3-6 b: bbjbbsbbb
7-8 c: cccstsjcc
7-10 j: njjjjjjjjjjjhjj
11-13 t: ttttttntttvtlttt
1-9 l: llllwlllllllxltl
6-7 n: fkqqxjnvcnvdwn
1-2 t: tkvgb
2-4 k: pkkbxpng
3-14 t: ttjtttttqxftttt
1-5 g: ggvhgjg
16-17 m: mmmmpmmmmmmlmmmhd
3-5 x: rrqzxxvnhxxrkpxnvx
6-13 h: hhhhhhhhhhhhhhhhhbhh
17-18 g: jzxjrphgpggsngblgg
3-5 r: kxrrrr
6-15 t: tjsgrvtttmtwgtr
9-10 t: rttttttttttttt
10-11 b: brbrbbjbbzbh
6-8 j: jjjjjmjrj
9-10 t: ptvtlzdntwmfbtdmvr
2-7 c: csccccrc
6-12 t: tctvtvsbkfkzmlf
2-5 p: plpppppp
11-13 d: ddddjddbdjtkcddcdd
9-10 l: zltlllllwcl
4-9 h: mgnhpxvrhthhnhhvh
3-4 d: ddddd
10-18 t: tttttttttttttttttst
6-8 m: mjmxzxsdb
5-7 f: vkffxfrqjf
14-15 x: cxzxvfxcxlxxxxcxxpl
9-15 r: rrrrrrrrjdrrrrp
2-9 t: tbtjbtttttl
6-7 k: kkkkkkz
4-5 w: wwwwfw
3-4 s: bjwsg
3-7 z: zzbzzzsz
2-7 g: tggggpggggcv
17-18 q: qqqqqhqqqqqqqqqqqq
6-11 h: phhhhnhhslkhhhhwchlf
6-8 n: ntgmnvsf
8-13 v: hndbnlvvmxvtvvndvlv
2-4 v: kcvv
10-15 q: vqqfqqqqnjvcqqngqq
7-8 z: hqkhzwrc
8-10 x: ghbnwkpvgx
8-11 m: mmmmmmmlmmjm
5-7 c: crpdsskcjw
7-16 b: pdddpflfpdbpghqd
13-14 d: ddddddddddddddd
3-5 t: ttqtst
1-8 m: pmcjmmdbkfmslrwgj
14-16 s: smsllbfzmthqsbkjb
8-9 w: wlvfswwvv
11-18 g: rqgnpgggggxggmgggfd
5-14 s: sxsszsssswnssv
12-13 c: cjcxccccsccktcc
14-17 c: cccccccccccccccccc
9-12 z: zzzzzzzzzzzzzzzz
13-16 r: rrrrrrrrrrrrwrrlrrr
7-9 g: ljrktgggcggg
2-11 h: njqvzqrtfxhzjb
6-15 p: vtjkspsxpqxtwrpqm
1-7 f: fffffkfffxfff
2-9 q: zqfkrhpsxkxtsxqlnfdm
13-14 k: tkkpjkcwnmkkkp
1-2 h: gmlrh
11-12 m: tmtzfnmckwmmv
1-5 k: skwnk
6-17 x: ggxmljccxsbxqxxxkxw
4-5 r: frrrc
3-6 j: jjjjjwljj
6-7 f: tzpnhcqx
3-9 x: vhbxpcgrn
13-14 c: ccccccccccccnc
5-6 b: bnjbws
11-18 q: qqqqwqqqqqxqqlqqqpv
3-18 c: cccccccccccccccccccc
2-10 c: clmfxqxcbcbgcx
1-5 f: qfhfcfflf
5-8 z: zdzxnzzzz
4-19 m: mmmmmmmmmmmmmmmmmmjm
2-4 s: nsqs
6-7 t: ttttttthttt
11-14 m: mmmmmmmmmmmmmx
9-10 l: zlllbxllllltlxlkkf
3-4 c: vcccc
4-5 t: nqttd
3-10 j: lxljmkdljbj
4-11 b: vcqhbhhqggzbmzwhmb
11-12 q: qdvrqtqltzqq
1-16 m: brmmdmmdmmmmpmwb
2-5 z: gzzzzmnt
6-10 m: mmmmqmmmfwmmmmmmm
7-9 c: hkfczdcvjg
4-6 g: jwghhkm
8-9 l: jlskbmkww
4-7 j: jjjjjjjjj
3-9 p: qtmsvbhnhp
1-8 k: kkkkkkkkk
2-12 c: hqbcnfbghhnn
1-4 q: qqqqq
6-8 g: gggggdgqggpmgg
6-15 v: vvvvvsvvvvwvvvv
15-16 r: wwrscsbhjbfrjmhq
7-14 f: zdtmfkrkjpfrfq
2-15 z: vzbxjpsfzlcbhrzdgjf
3-8 z: dsqzvgqmg
3-6 x: xxxxxxxnxhj
3-14 z: wlgkfrxzzgkxnzbbbvj
2-4 f: fffjffffffffffff
17-18 j: jjjjjmfjjjtljjrjlj
3-4 v: vvvv
8-9 m: mmzxmmxmm
3-5 q: mqqqqhqtq
5-15 p: pxpmdpppppppppppqppm
13-18 h: lhkhhhhhphmhhhdjhhs
6-13 k: kkkkkxkkkkkkxkkmk
1-14 c: ccccccccccccccc
1-2 t: tttt
1-2 n: txnnnf
11-12 j: jjfjjjjvjjkmj
7-9 b: bbbbwqbrbjwqz
4-6 g: nhrzdgrlgppkdnf
7-14 w: tvjwqbvwwdpxsfc
8-9 b: bbkbpbvhlbb
7-10 f: ftdnfdffff
5-9 q: kjqkqqmsfq
7-10 c: ccccchpcxgccccfs
13-14 j: jjfjjjjjkjjjjjtj
8-9 z: zzzzzzzzxzzzzrz
2-6 t: tttwttttht
4-6 n: tnqnnnntnbnsql
8-10 b: mfnbkrzbpb
3-7 x: xnxxxxxxqdx
3-4 w: wwlw
5-6 m: mmktbw
1-6 r: rrrrmrrrb
1-16 l: tllllllllllllllpll
1-2 k: kjklkkk
15-16 g: gggkgfgggtdgmggzgtfg
7-16 q: qqqqbqbqqqqlqqqpdqr
5-10 j: wzdrjznvjj
12-13 w: rzgplblzqwxqt
2-7 j: qzjjjkf
6-8 m: fmmmmqmj
10-13 x: xxxdxxxjfkxxpxxx
10-12 s: mhpzsbcwgjns
11-15 c: cqcccldfcgvctpccbtc
6-11 z: zzzzqczzzzfzz
9-17 p: zsdpgcfmbtshjcxpv
7-8 w: qwwwshwg
9-14 p: pqfrnghvtkvrmpk
4-6 p: ppppxp
10-13 k: kkkkkkkkkxkkgkk
9-14 z: zzfzzhzzlczzzslzzzz
2-5 p: jfgfspzhvzjfw
9-10 r: gjzftvgrrr
1-7 f: zfvffff
3-5 k: kkpbf
18-19 w: wxspglrvkvqndwvfjwr
10-16 m: mtmjwmvdmrmrhvcl
4-7 g: szdgggpqtvgngnxqgmzh
1-13 l: lclrlllsxzlnlkzrzdl
2-11 f: fffffffpffffff
2-3 c: ccccc
2-5 p: czpcrpmpq
6-10 c: cmcnvhcczh
6-7 v: gwvxhvvvcktjj
4-8 d: sgddqdkdddcwd
7-8 x: hxslvhdvxr
1-4 l: blmv
15-18 k: kkkkkkkkwkkkkkkkkkk
3-5 t: bttvt
2-9 r: rgrnnrrrrrrr
3-5 t: xstftt
5-6 c: cccczzc
4-6 j: jjzjsj
2-9 m: mdmmmmmmmm
3-16 c: cclccccccccccccccc
10-11 p: xcqgpwphtpw
12-13 b: bbbbbbbbbbbbpb
9-15 r: rrjrgrkmqrrrrrwrjvr
1-4 n: qdwnbnbxmf
1-3 j: jjqj
3-15 b: bgpbbwwbwbrbvlb
3-9 f: ffwffmfscffqfffdfw
3-8 z: sbzgdkpzvrzbhvvnhnfz
16-20 q: dqqzkqfqhkwqxktqqpxq
3-11 m: mmhmrpmmmmgmbmm
3-10 m: kbmpdsbpsmljdwmhctb
2-6 c: lhcccdc
7-14 b: bbbbbbfbbbbbwq
1-6 p: pppppsp
4-5 x: xxxwx
3-9 z: gbwzjjgzz
3-4 r: rrrjr
5-7 j: jjjjjjj
2-7 t: cknttlm
3-5 k: kskwk
3-16 g: kjgcfstgfgggggggp
1-7 t: sxqtvfttnlqt
2-3 q: qbkq
3-11 z: zzzgzzszzzzzkz
7-8 z: mwzzzzfqqzzzzpz
7-8 m: mzgmmmmmk
10-14 b: zwftbbbbbhwbbwb
2-3 f: kxffffz
11-13 s: ssspssssshssb
11-12 f: zzflvcftdfff
2-3 r: ljgr
9-11 x: gpxqrjctxlxbqxxxxxgx
6-10 d: nvdfddgddddddd
6-14 c: nkpqghqlkgzzcmcfj
4-8 r: rqvrrrlrxkrrrs
7-16 n: lnnlpnhnnnnnnpnfnnnf
4-5 l: lllll
11-12 w: wwwwwwwwbwww
9-10 g: ggggggggtggg
2-3 q: wqqqqq
5-6 q: qtqqvb
6-7 n: nnnnnnln
5-8 l: pvbglcrkvtpm
3-7 g: qgggqggggg
2-8 k: xcbfshmcsgjgmlrktm
7-9 r: drjrtrrrchhprrpqmtkz
1-4 t: tttttt
16-19 h: hhhhhhhhhhhhhhhhhhh
5-16 q: qqqqvqqqqqqqqqqqq
1-7 w: hwpwsnxpjnlsf
8-10 c: ccccrcxccnccwjc
9-12 v: mvvvgrrkvjjv
6-15 j: jjfjjfjxjjxbxbjk
8-13 r: rrrrrrrdrrrrmrqrrrr
5-12 g: gngbfxgtndxz
7-8 p: mvddjnppvp
6-7 n: wwnghrpnplznm
9-10 h: xgdvgqpghh
3-8 g: kgggwgglt
7-9 s: sssbsssssssrgz
7-15 f: sxhffzffhsfvxffhlf
7-12 j: jjqjjjjfjjjnjjjjjwjj
4-8 q: bqqkbqzqq
10-12 t: cfwtgmwjnxcd
1-4 j: gjjz
8-10 c: sghcqcwnscpt
4-6 l: dkxllllp
4-8 j: jjjjjjjjj
11-20 z: lgczxzzzzwbtmzznzzzz
12-13 t: rftntttdthtftt
6-7 z: pczzxhzcc
1-5 p: ppppppppppppppp
2-4 t: tctttt
8-9 x: vxjxqzxgjxwxxxkxvp
8-11 r: rrjrrrrrrrtrr
1-11 k: zkkkxbklxqkknjfz
10-14 d: bbdkzzwqvdqgbd
1-12 b: xvhdpgkbglcrxw
5-6 n: qsnxnnnnfrkbwhk
4-7 k: ckknfkdxlfx
2-8 s: scmsssssss
15-16 v: vvvvnvvvvtvvvvjbvvv
6-10 f: fffbftfffqf
2-3 g: gggpgwqgfd
1-3 j: jjqfj
11-12 g: gmnmgfkjhhgbjvjtjs
12-16 n: nwjtnnhnsdhnndnnvknp
10-14 m: mmmmmmmmmkmmmhm
8-12 d: ddcdmhddjsddddvdddbv
2-5 t: trpdt
3-5 n: wnhnpncnvn
2-4 h: zrfzcn
2-3 r: rrrmm
7-8 b: bbbbbbznbb
6-10 p: mpphpppjrppcf
11-16 n: nnnnnnlnnnxnnbnk
7-11 x: jwsrxpxwxkxqxhxmsvx
2-6 m: gsmkss
1-12 r: rbrbjnrxpwrr
1-3 l: jfqrbshllgzdmp
6-10 p: pzpppdhpprrp
14-15 n: ndpszbhnlqmdcnn
4-7 l: lzcpmlr
2-4 f: mlff
3-7 w: wwwwwwxwww
5-9 v: vmkwhjgnwwvdt
11-16 g: ggwgggggggzgbggpgg
12-13 z: mzzzzzzzzzzznzzz
3-5 z: xgzrz
2-4 g: ggggggx
4-7 c: tcltnkccckz
8-17 q: wqhbqqnzqqqqpmsqqq
8-12 k: kkkkkkkkkkkkkkkkk
4-7 h: thhhhhhhj
6-8 b: nbbbgstslzgvhrmvpw
2-6 m: mzmmmh
3-4 d: dcddbrwd
1-9 n: nnnnnnnnnnnnn
2-6 c: kcwbcc
9-14 c: clckqtcpcgpccckcwcc
1-5 n: bnnnnjnnnnnmn
11-14 g: gggvtgggggpggc
6-13 l: lvcjbjhljtvjfdkhclll
4-7 n: nnnnvngnn
1-3 s: ssdss
4-12 m: zrmmhmmmmmmm
16-17 p: pppppppbprppppppp
8-9 d: hdxdddddd
18-19 k: kwckkzrhckmnkksjkff
3-4 m: cmmhmp
1-2 p: pppp
5-7 j: jjjjjjjjjjj
9-11 b: bbbbbbbwlhqb
9-10 c: cccccccccc
1-2 b: qhpwfb
9-11 x: qxxxxxxxxxxxdxxxx
1-3 h: khhh
9-14 p: pplpppppdppppqp
2-5 l: wztllzqqcfl
1-6 p: wpppptpppp
3-5 x: xltvghxvtx
5-10 p: csgxpcnvhpzpbnm
5-7 t: ttttttmtt
1-3 m: nmmm
3-4 f: fsff
8-9 l: lxlllllll
7-11 z: zzdkzzzzqmzztgtt
8-9 b: bbbtnbbbjbb
2-4 d: lvdmdddxddcwdddwkvd
6-11 w: jjssdchrzkgf
1-2 f: fkpdbvjwmfbbvvgrgrxt
4-9 z: nbzzrczxv
7-9 k: lrskzdkmkp
8-9 g: xnbggmghwg
3-4 p: dwklppvpscfc
2-5 k: kkkkkkkkkkk
2-8 h: vpphcpsh
7-8 g: ggggggsgg
3-5 m: mkmpsmdqtgwwsqx
4-5 x: xxxxx
7-8 l: jsnhhkzqzthhcg
15-19 n: nnnnnnnnnnnnnncnnngn
9-11 m: mmmmmvmmmmzhx
9-10 k: kxxcbvhvkk
7-8 n: nnxnnnnn
1-2 f: fpff
4-5 g: gghjgwpggdjpggg
2-3 f: zxqkt
5-6 w: nvwgplwzrdcv
1-9 x: xxmvbtxllvxbx
9-10 l: llllsllfll
4-6 h: hnhchtthhhhh
8-11 q: ndqnqqqqjzq
2-4 v: nvvpvbs
9-13 r: rrdwrsnkrrrrrxrrs
17-18 b: bbbbbwbbvbbbbbbbbbbb
17-19 v: nbptxwjrjbfvzjqbfcz
16-17 n: nnnnnnnnnnnnnnnxgn
3-4 r: qrlt
1-2 f: skfff
3-6 w: wwwmwzwwwrwwrggnqbw
11-13 c: xrzccccccccctcvcc
1-4 s: ncvd
4-15 p: gqrppvkpqpdjzrpqr
8-16 g: ggqkgkgggrgggdggj
5-6 l: llllllsrl
13-16 p: ppppppppppppppppp
7-12 d: gqbflqdxvtqdznd
2-7 w: tqhhknlwhswgwbqwzcq
8-9 z: zhbzbzzzz
12-19 b: bbbbbxbbbvvbbbbbbbbc
8-11 r: hsrsdtrcdgx
2-3 b: bkbmmbww
7-9 s: wssscwsss
3-5 z: zszzz
15-16 d: ddddddddddddddddd
13-15 q: qhqqqqqqqqqwvqcq
8-9 m: nmqmqlvmmm
8-9 j: jljrjjjfjjzj
2-7 c: cfccccpccbccqccl
5-10 n: fxnxrlhjncv
2-5 x: xrxxnwxsdxgxxxxlx
10-11 n: nnnnnnnnnnnn
2-11 l: plhjdtrmslgzsszxcr
7-8 f: ffffgflfffh
12-13 v: vvvvvvvvvvvvvv
6-9 s: xsdgwqsws
4-6 x: dzhhlvjxm
5-6 g: ggggdqg
2-3 z: zpzzz
9-10 m: mmmmmmmmmm
3-4 r: fgqr
1-14 n: tznnnnnprnnnfqb
1-2 k: fgkjk
5-6 c: cwxcst
10-16 c: ccbjcnsbtdmccjhmcfff
7-8 b: dbbbbgbbb
4-5 h: hnhhc
1-7 h: hhhhhhwhhhhhh
1-7 z: zzzzzznz
4-16 v: vvvdvvvvvvvvvvvhvvv
5-6 w: whjwwwc
1-3 d: drdddhgdr
5-6 r: tgnlxrvprrwqzjlrgrj
9-16 j: jgjjjjxjjjrrjwmqs
1-8 h: gqbhphht
18-19 f: fffffffffffffjfffffv
3-6 s: qtsclx
6-7 b: bcbpbbbfhsxbbbnb
5-15 c: chcmcrbhxrscdccx
1-14 c: ccccccccccccccc
8-9 b: bbbdbbxhbb
2-3 t: rstxqmvhtbzlvg
3-4 x: vxjtzgcz
12-15 n: nnndnnxnnnnznnnnnq
2-6 q: frflkvwsjrsqlpvgv
3-4 c: cgbtqvhwcjsctjc
4-7 k: fkkqkqm
17-19 w: wwwwwwwwwwwwwwnwwxww
7-8 k: kkkkkkkk
2-4 r: wdxhgvqqhr
2-3 v: vsxxzd
5-7 b: gvbbbbb
5-13 m: mmmmmmmmmmkmmmmm
2-5 m: tmrmmb
3-13 s: sssssssssssssssss
11-14 b: hgqsbbgmbmnbbkbbtbl
15-18 p: ppppppppppppppppppp
1-6 n: nnsfcn
2-10 g: sdrpxhnzgbpcc
4-15 f: nvwxjnxhqvgkblpxf
3-4 r: rrlzjx
5-7 t: btrltlt
5-7 n: kgssnnn
4-7 x: xxxbxxxxxxxxxj
1-11 m: xmmmmmmmmmtm
12-13 q: qqqqqhqqqqqbh
1-4 m: qmmdmnmm
1-4 c: zchjq
1-2 c: nscll
1-3 v: lwfzvhjtxbdcsnt
1-8 z: zzzzzzzzz
3-10 j: hpmklqhwwqdtjf
1-2 n: khnngfnjpksckxctw
12-13 g: xggggmggpgggs
2-3 d: ddzd
7-8 x: xxxxfmpt
3-4 k: kkmk
2-3 h: hvknh
5-6 p: ppvppp
2-5 n: znnmlnqlnz
3-8 p: thpqdmdpkxfwp
5-6 z: zkzmzz
2-6 s: ssksssssss
3-4 h: hhhthhh
2-3 t: ttrttttt
2-6 r: rrfghrdr
1-2 w: fwwwww
1-8 q: qxscqtqqngkzhqjkhr
12-15 z: zzzzzzzzzzzzzztz
4-6 z: sljwzzhsfpxclrszzkhp
2-5 v: cbvdn
12-13 f: fffmffffffffxr
4-5 k: bdbkkkbxwjv
8-9 n: nnnnnncnn
10-15 g: xbbggswdwkhrklgfs
6-15 n: nxnnnsnmgnvnnnvnn
9-12 f: sffffxzkbrff
1-5 x: rjsxs
2-6 z: kmtjdg
12-13 g: gggggggggggkgj
2-4 d: mzcdgdd
3-8 r: wrnrhhsvg
3-4 l: hlplh
6-13 k: jksphjkkkkpxrf
5-6 t: ttjcgp
1-7 c: tskcxmrcwccc
1-3 l: llgll
5-6 k: kkkkgk
4-8 m: mmmmmmmmjmm
17-18 x: xxxxxxxxzxwxxxxxxx
13-17 g: ggbgfggggggggggtcg
4-5 d: ddrxvg
5-8 l: lllbxlljt
4-14 c: jcccccctfccccc
10-11 f: vzlbfvtdvfs
8-9 g: gvgmpzcjf
1-3 v: vnvvkfk
1-4 x: qxxvx
5-6 l: llllhllnlllllxzf
7-16 t: tttttjjtttttttth
2-6 n: jbpntjnnn
2-7 f: fffjfffff
4-9 s: nssmswbqg
16-18 d: dddddddddddddddddd
7-12 s: sssssssfssss
3-8 l: tzvljcvxdclnkl
17-18 r: rrrrrrrrrrrrrrrrsn
10-12 g: qxkxkmkgbgggg
8-15 k: gtmhfqzvkskzzvkfcd
10-11 m: mstltmqmvmmmxsnmmmmm
19-20 g: mgqgntggpmgwgbzwgjgp
5-7 z: zmvqjzzzzl
2-10 r: rvrrrrrrrqr
6-9 p: njhpszzmppdsk
2-3 h: hdhh
3-4 q: qrqd
1-3 x: qxlkjxmxmxxt
2-5 f: jffff
14-16 j: jxjjjwjjzjjjjjjjjjmj
7-14 k: skdkkkkkkkkkkkk
11-12 t: tfttqlntttctxfrvh
2-3 t: tcqkt
8-12 b: bbbbbbbbbbbbbvbm
6-12 d: kcddgcdwpmlj
4-5 g: gggttg
8-9 n: nnnlnnnxmxnn
6-13 x: rxtxxkswjxrmbr
6-8 q: qqqlqcgnqqhtq
13-15 m: mmcmhzmxmmmmnmvv
4-10 v: mnvpxbvtpvdjlsbwbvk
7-8 m: mjmmmmwmm
1-11 m: nmmmmmmmmmtmmmmm
4-5 v: rgcvvmngwffqdvsfzrt
9-17 k: kkkkkkkkskkffkhkrf
1-3 n: slgnnjnnnlnnnngnh
13-14 s: cpstfbkfstbhsjs
13-14 s: gqpsssksscshsscsss
19-20 t: tttttttttttttttttttd
4-13 q: clcssnsbrqqqnmczdv
10-16 j: jjjjjjjjjbjjjjjjj
3-4 f: ssnxlbvkmwfwk
15-16 z: zzzzzzzzzzzzzzhrz
15-18 q: qqgzkdjbzbzvjldmbqw
14-15 g: blrgqdsqqkqslggg
17-18 m: bbqdkghlszgkmqmmmm
2-12 h: hhzvgwsrxtchcpkxzx
3-17 k: xcfsrftlhmmcshkkkb
4-5 f: fcfsbffcd
4-7 x: xxxxxtxb
1-3 f: ffffvmwfzmfn
2-3 j: jjjqj
1-3 n: njnt
4-5 k: kkkqkkkkkkkkk
4-9 p: gqqsqgrwj
11-15 r: rrrbrrrrrrtrkrvr
1-2 z: zzzz
3-4 s: ssss
7-11 x: fxmxxxxxxtnxxxxcx
1-6 t: hrhttrtttt
11-13 f: xffpfnqfffrsbfnjff
11-17 z: zzzzzzzzzzzzzzzzzz
3-7 s: gssscvsshsnjr
3-5 r: rrrrjr
9-14 x: xxxxxxxxkxxxsx
8-9 z: zzzzzzzxh
3-5 v: vvvvvvv
16-17 c: mccccccncfcccqzcd
2-6 s: sssvss
4-6 c: cccqcwcc
6-9 k: hjbscbkkhk
7-8 c: wcdhcdwc
12-14 w: wwwwwwwwwwwwwfw
9-11 r: rrjrrrsrzrgrmrr
11-12 d: ddzdddddddqd
4-7 k: kkkkkkkk
18-19 m: mdmmmmmmmmmmmmmkmmj
1-9 p: wpppppppzppppnp
1-8 h: hhhhhhhhhhfhh
1-4 x: gxxxxwxxqx
7-8 v: vvvgvvxv
5-6 z: rzlgmn
9-11 q: qqqqqqqqqqqqq
11-14 s: sksswsxrssxmssssm
1-5 f: fxfrjfpzbddzfgtgqsb
10-13 p: chpppkpppppptpjpp
3-5 k: knqwk
3-8 l: llxwfmns
2-3 b: dbbbpg
7-11 f: pfpgqcslbclfr
3-13 r: rrprrrrrrrrrrr
5-17 z: zwzzzzpkfzzzhzzzz
5-6 p: tnpdzp
7-8 j: jqjjqlwj
1-2 w: wwwwwdwj
13-14 k: kkckkqkkkkkwkk
7-9 w: wwqwwwqwj
1-2 x: ljpxrx
9-12 w: wwtdwwwwtswjwwwz
1-6 k: skkkgjk
15-17 v: vvvvvvvwtvvvvbgvlvv
1-5 q: qqqqq
14-18 j: jjjgjjjjjkjjjjjjsljj
5-6 k: kkkkkkf
1-3 n: hvbfnbnn
5-6 q: mjqkxt
1-10 k: kkkkkmdkklzkx
2-7 r: rbdrvrmnqrjrchrffjg
3-6 h: hthtrh
6-10 l: crbmnfjbnh
10-13 m: mmmmmmmmmkmmjm
9-12 l: mlrnclqgblcxs
4-7 g: nklgggg
6-14 f: ffffxktfdffffqf
3-8 f: ffgffdfff
1-5 f: sffffffff
5-10 k: kkkkkkkkkkkkkk
6-7 z: zxspmzzjw
6-15 d: dzwfwdvcpzlrdfdd
11-12 m: mkmmmmmmmqmbmnmsmm
3-6 s: wfcsbstsbbcxwfbqb
16-17 v: jrvqvvtpqvvvvvvdk
3-11 n: nnnnlnnnznvnn
18-19 x: qxxwxsxxxzprvwxxdvxx
5-8 s: lcnsfgls
4-10 v: snspvprnvwz
2-5 n: rlnfmvtmblql
2-5 t: tvrvttt
9-12 w: lgwwwwrwrwwqzw
2-4 v: vvvhv
9-11 n: nnnnnqnnnlm
14-17 j: tjjtdwjjpmjjhjjjjj
5-7 b: lbrmbtbx
12-16 s: mzrhmvswtsgsxbpsj
3-6 k: rkzcngzkchksklh
2-10 x: xqrxxxxxxxx
3-13 z: zzzzzzzzscnzz
12-13 x: xxnjxpxsxzbbkxxxx
1-7 w: pwwwwwwzww
6-16 f: fffffwfffffkfffp
4-13 s: sssssssssssnsrss
13-15 n: nnnnqnnnnnnnnnn
4-5 x: xzxmmb
2-9 x: rlnchsswmhzqxqm
1-8 d: ddddddddd
6-8 h: hhhwhhhj
4-13 t: ttvvttmwtjfdltt
2-4 j: pdrpj
3-8 s: dsssssssss
12-15 g: ggggvgzggggpggk
3-4 f: fgxk
2-5 t: tnptttttttttt
5-6 j: rjzjxvjhvg
4-7 q: wmmqsxcs
11-13 w: wwwwwwwwwwwwwww
17-19 v: vvvvvvvkvvvvvvvvvvvv
2-3 p: pppp
4-8 t: ttztttttjz
7-9 z: ngzwkszkz
2-5 x: jxvjxbxtrvkq
6-9 l: nfpndlwllrlxkkltdllb
14-15 j: jjjjjjjjjjjjjjjjjj
13-16 n: vhhsjnlnmbthwrmslkn
3-4 x: xxtkfx
10-19 b: mbskbbbwqflrmxbbbbs
15-17 j: gsfldkvqsxmhmjjkjrs
14-17 p: pppppppphpppppppp
9-18 b: bbbmxlbbbxbbbbkbxtx
5-6 x: xxxxfxx
11-14 s: sjskslcsgmhhqs
5-7 w: rhhswqwwcrrqcnfdrt
1-13 m: xmrmdqvhhtbmmmtppmvm
6-7 p: mmlpzpp
6-11 v: hvqlncnfdnzjfzzvsg
3-5 v: tvvvvv
7-9 w: wwwwwwcwnwwww
7-9 b: dbbbbbbhbnbbbbbk
6-7 g: gggggvfg
12-20 x: hpwqtnbrtccjxmwwxsjl
7-10 d: xttrxddxndplzxcdzkf
8-10 k: xkkvckkkpbkbxkfmkbkc
3-6 b: gbcfzf
11-13 v: vvvvvvvvvvwvvv
8-15 n: ntwnncndnncknsxnns
6-8 f: fjcffffb
16-17 l: llljlllllllrlllllls
9-10 q: ktqqvsqqgn
3-9 d: ddbdddddwdvddrdkdd
5-6 l: lfllfllllx
11-16 w: wlwxwwwwlptwwwwcw
5-7 x: xfxxxxxxxf
13-19 z: zzzzzzzzzzzzzzzzzzzz
4-5 z: phfzzz
7-9 m: brmczmmmmms
1-4 c: ccccccc
4-6 g: prgghdcdgjggtg
13-14 d: xwddddddddkdfjdfdvdw
6-8 c: cccccgcptc
2-5 v: vvvvvvwvlvvvvvvv
7-15 w: wvcwvpcwwwwwwzspwwww
4-5 s: svsvgb
4-6 v: vlrvvv
2-7 t: ggkqlvsmp
1-4 l: mltf
2-5 m: lsmxvx
14-15 h: lhhhhhhhhhhhhrl
4-9 x: xxxxxxxxx
18-19 c: qjpfgccxcxjcmtdhccj
5-7 p: pfppxpdppsp
8-9 c: ccvcbwcct
2-3 n: bmnpn
9-13 f: fffcdclsbbcfclx
5-7 v: hfvlvtvfk
3-4 l: lllxll
3-8 b: nbbjrjmsgbfpdhz
13-16 g: gxxqgglggggggjvg
2-4 f: fbsq
3-5 p: wpkspnf
3-7 h: shhbtkcqvqhmt
10-16 r: mrgrrrrwrrmrrrrrrklh
10-11 k: kvckfkskktc
18-19 j: jjjjjjjjjjjjjjjjjthj
1-2 t: dzttttttt
13-15 p: zzpppprcpkpjppp
11-13 v: vsvvvjjvvvrvqvvlnvgv
7-8 s: smssssbs
6-8 c: cckcccccccccc
6-8 q: qlqqmqqq
2-3 t: cwhxt
2-15 c: zczcfftjdflpblcv
2-5 p: pgzclb
4-5 m: kmhwm
1-4 d: dddddd
2-12 c: ccmccnpcclccczzjm
1-3 g: gggg
2-4 c: tcqcj
5-10 n: rnnrnnnnwzsnn
4-6 j: gfczjx
5-7 j: jptwjjgm
6-7 b: krckbgbxbqjkbq
1-6 t: ttttttt
2-7 v: vzvvvvtvz
2-3 k: rkkk
9-13 g: ggggggggggggg
11-15 m: xpjwmktmtmmgmfm
2-16 q: kkwhqqcqnqqqqpqmqq
3-4 m: mmzc
5-6 k: kkkkkkj
5-7 v: zvbgvfvrphn
2-4 n: nncn
10-15 f: mzszfdffttmffjfh
1-6 t: ttdtgt
2-13 j: tjmswhmrttfxjrtffj
1-2 w: wwhwld
16-17 l: llllllllllllllllllll
10-12 j: jjjjjjjjjnjnjj
9-11 h: ghhqhsmhkhhwgrbfhhhh
3-4 s: sssss
4-5 k: kckxvvpk
14-18 c: cgcccccnwcccdscccccc
1-5 h: hgmwggcqh
4-6 x: cjbxcxxxcx
8-9 g: gggggggggx
1-5 f: jzfff
5-10 q: krnqrpqpqz
14-15 c: jvqfcssscxzjccc
8-9 l: lllllhlll
7-8 r: rzrzrmsrrcrbslc
8-10 p: lpqmmzppbvkpppczppsp
14-17 w: wwwwwwwwwwwwwkwwcw
9-10 c: ccccccccwjcc
3-4 p: pmptpp
15-16 h: jkhhfhhhnhhhhhhhhhh
3-4 j: jdjt
1-4 v: hvvdvvvvv
4-5 k: fkjbp
17-18 j: jjjjcjjjjjjjjjjjpj
16-17 b: bbbbbbbbbbbbbbbbbbbb
8-14 q: qnbvlqqqhqqqwqd
5-6 r: rrxrjj
3-5 x: qfzdt
9-13 w: pwmwwwwxwwwwmzfwwww
1-3 b: mbbbbbbbb
2-3 s: ssss
1-2 v: vvvvxg
3-15 j: tzhxjrnnhbjnwxz
1-4 q: dqqr
1-2 l: lllf
1-3 c: cpchqnmgprcd
10-12 g: gggggggggggf
9-11 g: ggggggggmggggg
8-14 k: xkkkwkkkkqkkkktz
2-11 l: dlfzbwlmpvld
3-5 v: vvbvvvv
6-8 p: xqtbnkkpb
2-7 b: qbcxbzdb
9-12 x: zlftxlbqxxfvrfr
11-13 m: mmmmvmbmmmmmmmmmmmqm
10-11 g: blpbhgjcgmgg
9-11 d: dddpddsnzdkqpdddk
4-9 f: xfcfthqzw
")

(defn good-part1? [min max ch pass]
  (let [min (Integer/parseInt min)
        max (Integer/parseInt max)
        ch (-> ch char-array first)
        pass-chars (seq pass)
        good-chars (filter #{ch} pass-chars)
        good-count (count good-chars)]
    (<= min good-count max)))

(defn good-part2? [pos1 pos2 ch pass]
  (let [pos1 (dec (Integer/parseInt pos1))
        pos2 (dec (Integer/parseInt pos2))
        ch (-> ch char-array first)
        pass-chars (vec pass)
        good-chars (filter #{ch} [(get pass-chars pos1) (get pass-chars pos2)])
        good-count (count good-chars)]
    (= 1 good-count)))

(defn count-good [good? input]
  (->> input
    str/split-lines
    (remove #{""})
    (map #(str/split % #"[ :-]+"))
    (filter #(apply good? %))
    count))

(def part-1 (partial count-good good-part1?))

(def part-2 (partial count-good good-part2?))


(comment

  (part-1 input)
;; => 456

  (part-2 input)
;; => 308

  (good-part1? "1" "2" "n" "anbcn")

  (good-part2? "1" "2" "n" "nnbcn")

  )
