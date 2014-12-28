(in-package :axion.game-utils)

(defvar *system* nil)

(defun get-path (sub-path filename)
  (let ((path (format nil "~a/~a" sub-path filename)))
    (asdf:system-relative-pathname *system* path)))

(defun read-data (data-type data-name)
  (let ((filename (get-path data-type (format nil "~a.lisp" data-name))))
    (with-open-file (in filename :direction :input)
      (read in))))

(defun load-texture (filename textures)
  (let* ((resource (get-path "res" filename))
         (texture-id (gethash resource textures)))
    (or texture-id
        (setf (gethash resource textures)
              (image->texture resource)))))

(defun image->texture (filename)
  (let* ((image (png-read:read-png-file filename))
         (data (png-read:image-data image))
         (width (array-dimension data 1))
         (height (array-dimension data 0))
         (image-data (make-array (* width height 4)
                                 :element-type '(unsigned-byte 8)
                                 :displaced-to data))
         (texture (car (gl:gen-textures 1))))
    (gl-bind-texture texture)
    (gl:tex-parameter :texture-2d :generate-mipmap t)
    (gl:tex-parameter :texture-2d :texture-max-anisotropy-ext 16)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear-mipmap-linear)
    (gl:tex-image-2d :texture-2d 0
                     :rgba width height 0
                     :rgba :unsigned-byte
                     image-data)
    texture))

(defun mouse-coords (win-height mouse-x mouse-y)
  (let ((mouse-y (- win-height mouse-y)))
    (make-vector mouse-x mouse-y)))

(defun unproject-vector (x y z)
  (multiple-value-bind (x y z)
    (glu:un-project x y (float z))
    (make-vector x y z)))
