(uiop:define-package #:app/widgets/yandex-metrika
  (:use #:cl)
  (:import-from #:log4cl)
  (:import-from #:function-cache
                #:defcached))
(in-package #:app/widgets/yandex-metrika)


(defcached get-metrika-code ()
  (or (uiop:getenv "YANDEX_METRIKA")
      "89257131"))


(defun render-counter ()
  (let* ((code (get-metrika-code))
         (script (format nil
                         "<!-- Yandex.Metrika counter -->
<script type=\"text/javascript\" >
   (function(m,e,t,r,i,k,a){m[i]=m[i]||function(){(m[i].a=m[i].a||[]).push(arguments)};
   m[i].l=1*new Date();k=e.createElement(t),a=e.getElementsByTagName(t)[0],k.async=1,k.src=r,a.parentNode.insertBefore(k,a)})
   (window, document, \"script\", \"https://mc.yandex.ru/metrika/tag.js\", \"ym\");

   ym(~A, \"init\", {
        clickmap:true,
        trackLinks:true,
        accurateTrackBounce:true,
        webvisor:true
   });
</script>
<noscript><div><img src=\"https://mc.yandex.ru/watch/~A\" style=\"position:absolute; left:-9999px;\" alt=\"\" /></div></noscript>
<!-- /Yandex.Metrika counter -->"
                         code
                         code)))
    (reblocks/html:with-html
      (:raw script))))



(defun reach-goal (name)
  (let* ((code (get-metrika-code))
         (js-code
           (ps:ps*
            `(ps:chain (ym ,code "reachGoal" ,name)))))
    (log:info "Goal reached" name)
    (reblocks/response:send-script js-code)))
