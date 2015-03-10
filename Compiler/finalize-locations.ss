(library (Compiler finalize-locations)
	(export finalize-locations)
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


	(define-who finalize-locations
		(lambda(x)
			(finalize x '() #t)))
	)