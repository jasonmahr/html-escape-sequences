module NamedEscapes exposing (codeToStr, getNamedEscape, subm, regexAll)

import Char exposing (fromCode, toCode)
import Dict exposing (Dict, fromList, get)
import Regex exposing (HowMany, Match, Regex, find, regex, replace)
import String exposing (foldl, fromChar, join, map, split, toLower)


getNamedEscape : String -> Maybe Int
getNamedEscape =
    flip get namedEscapes


namedEscapes : Dict String Int
namedEscapes =
    extractEscapes compressed keys


extractEscapes : String -> String -> Dict String Int
extractEscapes str =
    flip getEscapes [] << regexAll find "(\\w+)(\\W+)" << uncompress str


getEscapes : List Match -> List ( String, Int ) -> Dict String Int
getEscapes matches escapes =
    case matches of
        match :: tl ->
            let
                ( prevEsc, prevCode ) =
                    Maybe.withDefault ( "", 0 ) <| List.head escapes

                ( esc, code ) =
                    submPair match

                thisEsc =
                    if esc == "_" then
                        toLower prevEsc
                    else
                        esc

                thisCode =
                    prevCode + foldl (\c n -> n * 166 + toCode c - 522) 0 code
            in
                getEscapes tl <| ( thisEsc, thisCode ) :: escapes

        _ ->
            fromList escapes


submPair : Match -> ( String, String )
submPair match =
    let
        subms =
            match.submatches
    in
        ( headSubm subms, headSubm <| Maybe.withDefault [] <| List.tail subms )


headSubm : List (Maybe String) -> String
headSubm =
    Maybe.withDefault "" << Maybe.withDefault Nothing << List.head


uncompress : String -> String -> String
uncompress str =
    let
        applyKey match =
            let
                n =
                    match.number
            in
                join (subm match) << split (codeToStr (381 - n // 206 * 49 - n))

        shift =
            map (\c -> fromCode (toCode c - 390))

        bToZ =
            List.map codeToStr [98..122]

        expand =
            regexAll replace "!(.{3,4})!" (flip join bToZ << subm)
    in
        expand << shift << List.foldl applyKey str << regexAll find "([^ ]+) "


subm : Match -> String
subm =
    headSubm << .submatches


codeToStr : Int -> String
codeToStr =
    fromChar << fromCode


regexAll : (HowMany -> Regex -> function) -> String -> function
regexAll operation =
    operation Regex.All << regex


keys : String
keys =
    "Ȁ*6 ǎė ǎ4 ǈ!$%J ƾ· ƻ· ĝ&I:a ĕ+m Đ$Þ, Ö- Ô2 Ï# ¾7[ }#j'WV w/# q$~N6, g-*M `.*D ^3(2 Vǈ!IO T^!&D7 St4$ S/Ia S.7[ R7[ Q|, Qh< PE2 N6o(`# K-6E¿ Jm/% 8~*6#8 7î= 7IJ 7)J 7#!7!I 4Cg//V 4:J 3*+! /4# /#g/h$ -%&. ,ǈ ,D4 ,(+ ,&:*+J ,%(r3/ +M6 +'õ *3%6 )_, (?w/ '6M &<¼ %372%# $Ǿ #E( #;h< #6d #2* #/! !:4 !'(( ǰ5 ǐ5 ǎ5 ÆT Æ, g/| @>,¼± :4# 7Ϋ)7!I 7)I 2x# -`? ,Ǐ ()'/Oď &%~ #Ǿ #$3(/Ʒ !_, ǥ, ǜVr ǖ!'/{nM ǓN.( Ǎ!$%&$C Ì() ¹D# qM6!.$-& cB, _)/k''/} V¿&,7Ì R*&, 74# 4'W# 3'.( /V[9 /4() /)%- /(* /')Nb .3/ -B-*D +m# +MNb +'3/ )'Ȁ$-6a &:$&% &:!$ %-2 #Aǜ #Aǎ #!_ Ǔz*.3S/I Ǐ-D*(*7E >p# :{& 4C&W':$%2 ,Ǿ ,R/ +*! )d* (') !d* Ǐ-&V($+nM ·Ʒ q.!)rHǗ.B q)'+JW*($y n)2O jM6 c*88V$-nlc Wz6a E8d0 B*3Y 3I!= 33% +')M &<!0 #Ǖ #R7 #*(N !0} !$D$ Ǿ*(& ǖ).( fE3$- Vǖ4$-&:Y*( R++ 8*(:b /4lE) +{-V, +*C +!'/# (d'J= (*63% ()%-& ǘ#A w!)r n3Y K!, E(( /!*3 ,(?R )*6# #R/ q.!)rǗ.Ba!(t K!C 3*2 /!' ǌ.))9 q*!+E h:NS/I 7*6 /(~M +z~= $&%# 7!J d*,7)IJ&< ǘ$DV($ jY( S3l)St4 I.&$= (+! h$$|#3m(&' +4M= ǔ$6%nD$ h$$># 3N.( /!$+ ǈ!IJ$ St4$S. S)%-&9 2'W- #^3(2% ft~*7!*.3 $3/&r .3)# #8!I ǜVn+l c*I!*n+l I.&O ǖ!$+zY S.++$z( :4/'M 6!%DO *) %1 K1 '! $2 qM&'.!Ǐ-&$6!l, %(: +. Γ E8& ?. ,- ǿ ǉ ǈ4 2B &* %/ %) #) ǒ #ǥ, ǚ Ǜ ǋ '/8 &! Ǌ &# $, $? 4! %-6 ǔB _ ($& %//!'Ǿ $( (+C ǽ $! /).( Β Ǚ (. ǘ0 +*!+ $# *- '- ǚ*)2$ Ǉ Ǳ %+ Ǌ'.7E (*3 Ǎ!$%&$! )$ Ǽ !# '& 7'Ǿ Ǌ'ǽ- Ƿ ǜ$+&'! #ǥ# !*%-6)$ ǒ$8& Ǯ ǋǷ.%) Ǭ Ǩ ǭ +ǿ# %! ǳ Ǫ !'ǽ *ǭǮ& Ƕ ǻ Ǵ ΐ ǩ ǯ ǲ ǹ ǵ Ǻ ǧ ǫ Α _ Ǹ "


compressed : String
compressed =
    "h%7Ιǔ$WjNOŅ+)ΧǗgǕhi-.3#2'))4#/V+-bKǓǖim'(kŕ!ŕ%(bÕ%(&,U#Ō#/V*'2TĂ#ôΛ($3*#jhi`.l(#ǍhitYbŌb)(?ĵJ,7Ă#!(?řJ,ǎ%b)'Wğ°µǍ!%Da)wĵaDVb¶jNaDV74,!wřaǔMŹN6S/I$γ-7(/,*Ņ+)#+$-b/'.-2#w!!$-#r$-#7!Dğ($+b2*OHģ.3),ģqǕǖǟi{28k%t'#-B#(:r#ǘfǍišǘ,3ICǕDVp,(d-(,2$6#ìĨ#U3-,/3ąƸØƹ#´µKw&aŖ'#/4%#q$-&VcBiÕo,qz~)%#+z~ąƷ#{23#!%t'ÿƺ#:l8ÿƸ,8!Iƹƺ#*tYbK°K´KP#KüK¸K!N6#KfÙq+z~#f°f´fP#f¸Ǐ°Ǐ´ǏP#Ǐ¸fhǎ#ǔüǕ°Ǖ´ǕPòüǕ¸Þò()x#g°g´gP#g¸ǟ´hǎǕǘǔ#(ȀÙ%°%´I*!+#%ü%¸4N6#%$Ù++z~#$°$´$P#$¸*°*´*P#*¸$&:#-ü'°'´'P#B~2O'¸2*DŁD*2a'()x#.°.´.P#.¸r´&:{-#r¸KöKŜKň=qÇqP=qo=qÄcÄcãföfovǥ#fň=fÄǍP=ǍŜǍo=ǍÎǎP=ǎãǏn)2$=ǏöǏňvǥ#Ǐo#N'o#*3%&:ıǐ)*6=ǐP=ǑÎJ6!$$-#jÇjÎjÄj3*o=jãǔÇǔÎǔÄ-m'(#fǔǍ=ǕöǕ2Ĵvǥòf)*6=ǘÇǘÎǘÄSÇSP=SÎSÄhÎhÄhãgn)2$=gögŜg!N6=g2Ĵ=gň=ǝP=ǟP=ǟ¸ǠÇǠo=ǠÄ8-'8Τ*3/zγ6I.&$ϐǰ3%&:ϒPПǎI$J#+4MŒîΡīo#µģ!N6#ň#µL#n)2aµHKw&O2Ĵ,@ǈîτg-2VpαK)/:%ϯǈÍǍ%õ#c$)&%#fűǠÍf&%#h:ÍǏB%#Ǒm/%#jņǓ.#ǔ.#Ǟ*òŖM#ǖ*#ǘ:'#S*63%Th%.#gűǖ:*#q:*#ǖ(*ò3$6%#l/:%Θ7Í6%õ#2$)&%#$ű$ęDő$Ï,ȀÍÍ&:Í*B%#Şkņ3.#-.Į*#'ŖM#/*#!:'#D4â#âD,â8,â#&%.#.ű.ę,/:*#D4/:*,/:*D,+:*#ę#'3$6%#đ(r3ΘD4đ,đD,.ę:#gę,(d%0/:*v/*D#D4/*,Ǎ%õ2ΖǥŁ6%õőŞΣŞD,D_:'#!:'D,(d%0$ÏΔ$ę,ś$ű7$ęıǕ+rΛcļǍļǐ.J5cS5Ǐ.J5ǟǏ5ǐ(V5jļǔļhSĻǑļg7!+rTcǠ5K5ǈ5ǜ5Ǎ5c5Ǐf5ǠĻǠ5Ǐ5ļǑ5j5Ǔ5ǔ5Ǖ5ǖ5ǘ5S5h5g5ǌ5ǑĻhS5qĻSĻSǎqĻǎKǘc5ǟ5SǕǌh5f5ǟg5ǟK5%575D56525*$5Ȁ:5Ȁ5*5ĽJ5)535-5'5/5!5(5&5.585J:5&(5+:5(:5(:+:5:425r5('8&5$5r.5r%5*'+rT2Ľ6Ľǰ.J52(5*.J5r*5ǰ(V5)Ľ-Ľ&(:5JĽ.7!+rT2Ȁ5$-(/κϷ$3(/ĭƹĭƺ#-.3(/T/.-+(/#&:N(/#ÑaĪÑO:%*!(/,ÃċOÃh:*+JŦÃĪÑaǠV'ǝ*2&:ŦÃÑaȀW-ǰ#ȀWǰk!3#!)3#ĳ:r/:$-s2xv3ĳ:{ğǜVbǜV74,)(t'TǕ/$-×'#q)'($×{,)(t'C(7t',Ǖ/$-þ$T)2t',!2t'C!2t',q)'($þa72t'k2t{,2%66VT22%66$Cc%66V,7.)Eb7.))s)2!v3)2C:$))*/,/V3~Κ/V&$-J#ÛOǖ!*3O&ÛO7ÛOśÛa)(%t'Δ!(%t'#')N$Δ+4$&v:r7.))T8!%()#7($3*Λ?Û$Θċ$Θǔ'Ź#K//)rǌ.-+nM#%8,*bĊh*3Y,*+#Ċq'õ,$.!'ϙ&oοh!*/EģcBcB#ēEǾYζqe,N+4$v6ÆΕ:%3~bǎ~7V&ŦǎĹǎ8Cǖ'N+4$Ěaǎe#t%&V-*M(,Ě+J:#:ğĚJD,:()x,Ě+JıXŋ)NaŋOǏ8!ı3,ŋ/4&,jm)I$d8k%6!%-,jĹ$))#œ!l(Tǔes.3V'#+'/r(CW$*V/#W/,ǖe#ÛY,!%nMl(#Ǘe,!$lNOǘĹ!$l/4bǘa!$l,ǘ8!,ǘe#!$l(,!Ǿ#hǘKcfΔīN&$6V(TǠe,':3T3:'#Ǡ8CȀ$$d8,**B%#^(&TǈX7V-'.ŒV-'.))*(,q%rEr(#q8!,Y+!TŅ/$+&%nM#fĹǌ'.!*Vd8#ǌĹǓ$))Nd8T/:õ&,ǓĹ'X{2V'8,{2V,%E/:#%E8(r3,7$&:#6*3$)#2%E&:#qm*&lúΝcc,ǥ#ú,fǾ/M$-nlfi$a**#Ǐ3%6N4rǏ,8!IƷƹΛ8!IƸƹÿŷƸŷƹŷƺƻÿƼ·ƻƼÿŸƹŸƻŸƽƾ#;K!1ςīSĈ;K!1,Ŋ(Ŋg/K!1i._,SĈg/K!1,QK!1iĬ(ĬSĈQK!1,@K!1iSĈ@K!1,2_,;QK!1i:_,g/@K!1iD_sWŘ;K!1sW41s$ŘQK!1s$41,($41#j'WVQK!1,($_,(W41#j'WV;K!1,(W_s)4C-u41s!4C-!041,!_WT!0(t*641,jću41,g4Cǘć!041,c4Cu41įk_&),!_&)#!041į,;h$$K!1#3m(&'u,3m(&'.Ŕ$K!1,Qh$$K!1#3m(&',3m,@h$$K!1#3m(&'¼,)_:JT:''Ju41,!_:J#:''J!041,)_)/k''/41u,!_)/k''/41!0,ø(t*641#:_Wsø41#-:_,j(:Tīǘ(:i)2(:#!2(:#+!_Tw)4Cw!D$41u,w!D$41!0#wĬPE41uv'ŊPE41!0#{_,;>#u±./,):4.,u±¼k:42,@;>,./±!0#Qg/>,.:_,./±E8b.:4),;g/>,Ŀ.#Q>,!0±./,Ŀ2#!0±¼,@Q>,2:4CQķ!0,2:4)#;ķu,!0u41(#QK!1;K!1,!Ŋg/K!1@K!1#.2_,ø41(kĬ;K!1QK!1,))4Cuu41(,././41(#.._,!0!041(#!Ĭ¼¼41(#22_,ÊºkĿ,ø±(,!)Ķ!0u±(,ºs)ų;41s:ų;!041s!ųQ41,H;K!1#;41,)K!!,g/41#Hg/K!1,.K!!,!ÖHQK!1,Q41ı3/)*Y,@41#H@K!1,2K!!,:ÖH;QK!1,*88,;!041,DÖHg/@K!1,g/¼41sWų$Ö($Ö(WÖju41kK_,ǘ!041#!K_,ż!_T)_7Η;K!1p,QK!1pČ7,2._Π@K!1g/K!1,)'_Θ!'4C:'4Cǌ{K))iēE3$-bē,ǖ4nlc#/4&,Ņ*(bfí(,]fí(#-$ís$í(,¹#¹[,¹Dő-B:N6s%7)%Tc$),*(NðD,ë&,NsBN#]ë&sBND%,Êë&T-*s*D,S.+:h:%&,]ÊëbĖ%sB-*,ǖ!'2.+&vŲ,+'Ų#q'Ų.+&,S.3iÁ#3-U#3/,Ĩì,oU#U2',[3-T[Á,([3-,(3l)[ÁŒIJ()x,)'W%(bē8-#S3l)Ò,!%2*+TS?!&,Ô/&'vĩl,Ô/őÔ/&',DÔ/,N8N#^!b^#^E,Ū#3$%R!z^E,^(/:#Õ#(Õ,(ĈÕ,¶p,]¶p#-(ĈÕsÕs(Õ,ç#H¶p,(/4,/4,(Ĉçsŕ-ç,]H¶ps(/4s(Ĉç,ď#ù'CD$aĕŮNbǏ-&$6!lı-b&Nb**N&,Ĕ'N&,yqMNbHyqĔ+WNb+WĔý%WĔq'.-&Výh:V$8{Oī&:V$ƺŒ$+%.(Oī7$+%.(,!%n'#q')M#ĩ,oÁ#Á2,3ccBT:'3&:b&:JG#GŏG,L,7G#śG,3(&/'(#I,I2#WC¶L,W!$%&:sG#]L,`G#$G,9L,G`#GaL9sG`#]L9sGaō#LÓ,G-O-ō#]LÓŎ#LL,Z,&:JmŏZ,msm#-Z,]LL,Z`#mam*2#7ō#śōŎ`#q./qm,7ė#Ż@ŻŒė`,ǥ#Ż9,7ėaYo#cB9,o`,o`o#$ģ8l)Š$8ģ!*(ŠVģô`#K((*6-,ôa$ô#`ô,$å`P,ĄOP`,Wz6`TD$$`#d*$T&<?,tY&`v`.Y&sO]9,ū#Ĥsū#]Ĥ,ETE?,F9#6`,6aÉÓkf,E??,FÓ#6f,6`?,)-fkĲ,6Ĳ#6-f,ǔY&zÉÉk),j&,ǍbǔY&zFF,66,&W*Ǿb7$&W$$-,]q./qm#-)b-Ü,]É,]ħ-6&s6d,]É9#-E?sEs6O-6`,]F9,)G#ÉL,ÜG,6GłG,FLs)G#]ÉLs6G#]FL,)6#Ü6d,ÉF,6)łÜ,FÉs&)6#]ÉFs&6)#]FÉ,/C³,À,é#²Ő,/!wO³½,Àß`,éß`#²½Őwa/!G#³L,ÀG,éG#(+G,²LsÀ#-/!,]³,]²#-és(+,Ťñą#S./V[ą[sR7#-R/ñOť9,Ť`ąOR/[`,S.ĜsR7O]ť9sŤ`,]S.Ĝ#-R/[`sR/aR7-OŤ-`ą-OR/[-`,w/oT.U#ŬìÚ7[#ŰÚ7Ú/[#¾/V[Ú/Ú7O(?Ť`,Ű9Ú/[`#(?R/a¾Ĝ,(?ĕŧĀ,ŧŬ#ŉ,'U#Òì,ÒĨ#'Á,÷#Òh*3Y,'Ă#ÒcB#'o,'åšP,'%(bš%(&,'2xTš2x,U7#AU,Á7#AÁ,Þ7#AÞ,(o7#o(t4aQh$OD2x,;h$O2xD,&'/#@h$a/VŔa7B&'3,7B,3'2$)(THQh$ODcx,ǜĳǜDĳǜcx#-Dĳ-Dcx#-ǜĳ-ǜcxŀ.!$)ŃTD4&<u,Dă,Ţ#Dāőó,&<u`Ń9,ăaŢ9#āaó`,{*6'8#*3'8#3.)n3m#3.3m,:V+M#N&+l#N&V+l,D$$ğ74D$$ŨkāĮWz6Oǝz6aÐùÐD$Oǜ$aǾD$aĀ#Ð+mĆ+mĆŮŬ,Ðw/,c*%3M2i2*%3,(o#S&4#((&48,2*DMǾŁD*2$MÞ,7'WnO)Þ#!ÞkĐOuŴ!ĐO!0ŴśG`#7GaßD$OwD$awWz#ßùS.7#ť,S./#S./[,qm#q./#8{J#/*&+:8{J,$/4k&o#Üo,6&oło,j)#666#Ǎ6,E6#É9F,Ü`6d,6d`Ü#F9É,6$),ß`Àvw$/!,wY+#ß`é,]³½#-/!wa]²½#-(+wa-(?R7O]Ű9,]¾Ĝ#-(?R/a)-Gv6-Gŀ-G#À-GŐ-G#é-Gsă#-&<u,];h<sā#-ó,]Ţs&<u`#-ăa];h<9sāO-ó`,]Ţ9,D$))*/#+&o#.&o#2&oŁ(Nð(Dð(ðo#-BND+#-BND7ðfT-*(2Į-*(#-*(#Ė+#Ė7#74WzΗ74ùpWz#2'.7E74ù)+$~T;ŭ!+$~#Qŭ;ǌ)''C)8)'{,!8)''CQǌ)'{,2!ä2)ä.!ä.)ä7-B#Ô8)N$TÔ8R!8#&$)!$+T&46$b.)+{-Ζ.)æ.!+{-#.!æ2)+{-k)æ2!+{-k!æ81-v(81-,(3*E#((3*E,+r)+&rΚÔ8l4#&'/7BΘ'D74ΗĂ74T^Ȁ_ύ)ĝ&τ)Ŷ!ĝb!Ŷ&ÌvǕDġ#g-2ġ&ÌòDêζg-2êòDũg-2ũd/$Ȁ*.3v$)N&V(Ε7)%-Jό'SеšS,A:ψǎ{*ȀM&ljNaADTA2!ΚA2)ΔA.!ΔA.)ΔAD!ΔAD)ΘA:2ΘA:.ΘAD:ΘAǎΤAǜ#A2àcCAcà2j#Ac)#Acj#A.àgCAgà.j#Ag)#Agj#ADàǜCAǜàDjĎ)Ďjč2#A:cčcč.#A:gčg#ADǎĎ:Ďǎ#.:ŚΤ):ŚΔ7)'+JΔŚƷƺΙŚƷƸ#Śƹƺ#ŧΞī(t,(t48ΙĴJ(t4a(t8,ǌ~E2ĪÈaf3/&rĪÈO!$+&T34J$C8)&-(vÐ&<./TǾ.Ë#.d*8,&<#.Ë!0vā8,ā#óĆ2d*ΔÐŇ,2d*8#ĴJŇ,Ň#2Ëuvă8,&<E8bă,)'ȀΗĒåd*oαǾPvÐP,.ăΙ.ākă#f3/&rÈOǌ~E2ÈO(&48ΙÐ(&4,(&4#/:M$Θ8$3%Eς3%ET(/%2Yή(/%2$Ġ+).7(v+).7Ġ:$4&(T:$4&Ġ2*%3(Ł%3M2ĠR-6Δ8)%&vœCœ!l,(:4/#+:$+J34Jд+:$+J,+ľΔ3l&Y$Ι3l&,(Ņ&Φ¶S$/4%&{β)7ÌΪ!7Ìk'ÌЃ;H¿&,QH¿b!'Ì,)^#;ş&,)^E,Qşb!^E,!^,j^#ǘ^k'^#!'^#û;K!1ΘǥĆŊûQK!1iǾĬû;QK!1iǾ:_,Hû;K!1Į)K!!,ûu41,HûQK!1#û!041Ć!K!!,Hû;QK!1Į:K!!,ûø41,)M63m(&'TǾ3m,2ż!_v-D)Öϭ-D!ųDźCǓmk7_Η7J}#!7_,)pC!pC27J},2!7J}#ǘǈ_,ccB!%:2#g/K!1p#@K!1p#ǘ_&)v)%įv!%įkKį#!Kįk_8(Č8(k_78(Č78(#-Wŗv-$ŗ#:J($}#($ŗ,:J(W}#(WŗsW-$4#-Y$4#&'$%,&'(%#(YW4,(W-W4Č+Ιw2_!T)2+%#!2+%#w2_)k_/)#w!_3vw)_/Č/)Θ:_Ąvg_'å).!2(:4k2!.(Ķ;Q>vQg/@>#@;Q>#;g/@>#;ĉQĉQg/ĉQ@ĉ@;ĉ@Qĉ;g/ĉ;@ĉ;ÂQÂQg/ÂQ@Â@;Â@QÂ;g/Â;@h$$>kź#.ź#!ź#2źk.!.:4k2!2Ķ!.).Ķ!2)2:4k:4.)k):42#Ŀ.)kĿ2#.2Ķg/º,2.ĶÊg/º,ǘ'.-2Ǐ3/)*Y#V4CG!4C)_GČGČmk&)4C6d_TR7!4CR/)_T)è!è.è2è)'/4Ζ!'/4kÌ$Ε!ÌO)Ħ.#!Ħ2kĦ2#!Ħ.k^2#!^2k/4)b!/46b6&)ǖ4kdǖ4#DżȀ%6ΔD^!&Ũ2#^$Η!^O2W^E#.W^E»%»7#ŪI»2»OŪ%8»6»:#7ĥ2ĥ+ĥ!%¹Dk%ĥ':ğ'Õ#'ŕ'/V/T')+ľT'2Ă2#')ĄT'8å')b'6bĄ(åĄf#Ă7#7Ă7#AAΔd*(7Δ!d~d*Ńp#Ţp#!I$Κ*N8NTN8NnO-DN8N#$ěΕ(3$ě#`Děk'Ȁ8ΖĴJĒǘ.Ec$)%rzΙ2ĂTÐ'oΚǾ'o,Ð'UĮ'UĆB*3OÐ÷Ć.UTÐ.UĆŉTÐŉ,***N&Ζ?N&,8/4&NbĄ8-N&v%WNb!/Ę(+Ę-Ę/'N&Nbt%&NbN&)ŗ#UĄΛUI*CGU#U2.#UG#U&W'#3ŌTÁ2.k'Uv!'U#qľ#Þ2#Þğ(3x/T)÷#!÷#÷%(òÞ#'2*D#d*U#d*Á#d*n3O*Ų#N&Ų,%3l6v+mo#-w/T-ŵď#w/'Cw/ŵŮw/7!ŵ7!Ůw/Ů+mĕ+w/(#++m(#+w/((3vK-2vǕCďď#{'C{İİDT{D#ď2#{2#Wz74T(o$ΗGoΔōov$%(&$CmI*Cmf#$U#UOfG#q')MO9#$ccBT2o(`,ūcck&å6&å)&tYb6&tYbÉ½ń,E?á,6Y#6`á,F½,E(o#6Yońo'#6Yo'ńo'C6Yo')km#ÜZ,6młZ,)-`k-a6-`#6-a)-Zk-m,6-m#6-Z,Ü`?6&C)f6,6f)ł`?Ü,)GO6GO)G6#6G)k6f#6)fń6Y#6YE(#`áÜ#$)(,$6(#`á6d,$)(o#$6(o#$)#$6#G)vG6#G)f#G6f#ÉÉ#Fħ6)ǰT6)%k&++#6&++ń++#6Y++#(3b)%b(3&O)%&O7ėf#³9ŀaÀ`,²9#(+aé`,/!fv(+f#ÀĲŀ-fŐ-f#éĲ,ÀZŀm,(ĕéZ,/!-m#À-ZŐ-m#é-Z,ǖCS+ñoØoñUØUñ3.)bR/3.)bR7$oØ$oñ[`?ñfą[`?Øf,R7GØGñ[ĲvR7-fą[ĲØ-f,+R7v+R/#+R7O+R/OR7R/ØR7ñR7ØR/Ø:R7Ø2R7#8{JD#&e{J#3)+/#H;h$$ΙcxD,ǜ2x)TpD#Dp#DpD#ǜ74T]#7]#!-Õ#ĄÕ#Õå&'/å-:ŕ/4G#ěΚ88ÙЪГŝ)Ù8ŝ8)ÙKĺvϮqĺcXǍÆvǐÆvǑXǔÆvǕXǖXǗXSĺhXgXǜXǝXǞXǟXǠX%X7X+X2X8ĺ:ĺ*XǰXJX)X3X-X/ĺ?X!X(X&X.XDXWXǾXrXȀXK8!υǈ8Cc8!Tf8Cǌ8CǍ8Cǐ8!vǑ8Cj8CǓ8Cǔ8CǕ8Cǖ8CǗ8CS8!Th8Cg8Cǜ8Cǝ8CǞ8Cǟ8C%8!TƧ8CƧ8CKe#ǈe#ceTfe#ǌe#Ǎe#ǏeTǐe#Ǒe#je#ǓeòeTSeΔhe#ge#ǜe#ǝe#Ǟe#ǟe#%eTƧe#Ƨe#"
