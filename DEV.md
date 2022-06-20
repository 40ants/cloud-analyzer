How to build app-deps.asd:

(real-main "app" :except (list "yandex-disk-cleaner" "sb-bsd-sockets" "sb-cltl2" "sb-introspect" "sb-posix" "sb-rotate-byte"))
or

qlot exec gen-deps-system --except yandex-disk-cleaner,sb-bsd-sockets,sb-cltl2,sb-introspect,sb-posix,sb-rotate-byte app

Yandex Metrika Goals:

- Показана диаграмма использования места
  ym(89257131,'reachGoal','diagram-shown')
- Показана ошибка
  ym(89257131,'reachGoal','error-shown')
- Запущен пересчёт места
  ym(89257131,'reachGoal','disk-size-recalculation')
