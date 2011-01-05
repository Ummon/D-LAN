// all aybabtu is contained in this object
var aybabtu = {};

aybabtu.rot13 = function(chaine) {
   var ACode = 'A'.charCodeAt(0);
   var aCode = 'a'.charCodeAt(0);
   var MCode = 'M'.charCodeAt(0);
   var mCode = 'm'.charCodeAt(0);
   var ZCode = 'Z'.charCodeAt(0);
   var zCode = 'z'.charCodeAt(0);

   var f = function(ch, pos) {
      if (pos == ch.length) {
         return ""
      }      
      var c = ch.charCodeAt(pos);
      return String.fromCharCode(
         c +
         (c >= ACode && c <= MCode || c >= aCode && c <= mCode ?  13 :
         (c >  MCode && c <= ZCode || c >  mCode && c <= zCode ? -13 : 0))
      ) + f(ch, pos + 1)
   }
   return f(chaine, 0);
}
   
$(document).ready(
   function() { 
      $('.gallery a').colorbox();
      var mailTo = aybabtu.rot13("znvygb:");
      $("#emailLinkGB").attr("href", mailTo + aybabtu.rot13("tert.oheev@tznvy.pbz"));
      $("#emailLinkHM").attr("href", mailTo + aybabtu.rot13("ureir.znegvarg@tznvy.pbz"));
   }
);
