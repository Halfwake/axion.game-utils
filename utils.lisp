(in-package :axion.game-utils)

(defvar *system* nil)
(defvar *executablep* nil)
(defvar *tolerance* 1e-7)

#+sbcl
(defun get-running-path ()
  (namestring (truename (cl-fad:pathname-directory-pathname
                          sb-ext:*core-pathname*))))

(defun get-path (sub-path filename)
  (let ((path (format nil "~a/~a" sub-path filename)))
    (let ((system-path (asdf:system-relative-pathname *system* path)))
      #+sbcl
      (if *executablep*
        (cl-fad:merge-pathnames-as-file (pathname (get-running-path)) path)
        system-path)
      #-sbcl
      system-path)))

(defun read-data (data-type data-name)
  (let ((filename (get-path data-type (format nil "~a.lisp" data-name))))
    (with-open-file (in filename :direction :input)
      (read in))))

(defun load-texture (filename textures)
  (let* ((resource (get-path "res" filename))
         (texture-id (gethash resource textures)))
    (multiple-value-bind (id size) (image->texture resource)
      (or texture-id
          (setf (gethash resource textures) id))
      (values id size))))

(defun image->texture (filename)
  (let* ((image (png-read:read-png-file filename))
         (data (png-read:image-data image))
         (depth (png-read:bit-depth image))
         (width (array-dimension data 0))
         (height (array-dimension data 1))
         (size (array-dimension data 2))
         (colors (if (= size 4) :rgba :rgb))
         (pixel-type (if (= depth 8) :unsigned-byte :unsigned-short))
         (image-data (make-array (* width height size)
                                 :element-type `(unsigned-byte ,depth)
                                 :displaced-to data))
         (id (car (gl:gen-textures 1))))
    (gl:bind-texture :texture-2d id)
    (gl:tex-parameter :texture-2d :generate-mipmap t)
    (gl:tex-parameter :texture-2d :texture-max-anisotropy-ext 16)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
    (gl:tex-image-2d :texture-2d
                     0
                     colors
                     height
                     width
                     0
                     colors
                     pixel-type
                     image-data)
    (values id (make-vector width height))))

(defun mouse-coords (win-height mouse-x mouse-y)
  (let ((mouse-y (- win-height mouse-y)))
    (make-vector mouse-x mouse-y)))

(defun unproject-vector (x y z)
  (multiple-value-bind (x y z)
    (glu:un-project x y (float z))
    (make-vector x y z)))
