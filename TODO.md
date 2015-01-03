* TODO [2015-01-03 Sat] compgl: Handle redirection syntax

  When command-line is 'foo 1 2 </some/path' bash will supply COMP_WORDS as (foo 1
  2 < /some/path). We currently do not yet specifically handle this.
* BUG [2014-12-13 Sat] periscomp, compgl: completion of short option belum benar

  - harusnya -a -> -ap -> -apq (semua short option yang belum dimention dan tidak
    require args), ini langsung complete -a <spasi>.
