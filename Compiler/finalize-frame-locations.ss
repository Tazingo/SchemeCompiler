(library (Compiler finalize-frame-locations)
	(export finalize-frame-locations)
	(import 
	  ;; Load Chez Scheme primitives:
	  (chezscheme)
	  ;; Load provided compiler framework:
	  (Framework driver)
	  (Framework wrappers)
	  (Framework match)
	  (Framework helpers)
	  (Framework prims)
	  (Compiler helper))

	(define-who finalize-frame-locations

		(lambda(x)
			(finalize x '() #f))
		)
	)