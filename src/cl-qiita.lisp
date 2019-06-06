(defpackage cl-qiita
    (:use :cl)
    (:import-from :cl-qiita.base
        :http-get
        :http-post
        :http-put
        :http-patch
        :http-delete)
    (:export
        ;; likes
        :get-likes
        ;; tags
        :get-tags
        :show-tag
        :show-user-following-tags
        :delete-tag-following
        :get-tag-following
        :put-tag-following
        ;; users
        :get-item-stockers
        :get-users
        :show-user
        :show-user-followers
        :delete-user-following
        :get-user-following
        :put-user-following
        ;; item
        :get-authenticated-user-items
        :get-items
        :post-item
        :delete-item
        :show-item
        :patch-item
        :put-item-stock
        :get-tag-items
        :get-user-stocks
        ;; authentication
        :get-authenticated-user))
(in-package :cl-qiita)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        likes           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-likes (&key item_id)
    (http-get :path (concatenate 'string "/api/v2/items/" item_id "/likes")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         tags           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-tags (&key (page 1) (per_page 20) (sort "count"))
    (http-get :path "/api/v2/tags"
        :query `(("page" . ,page) ("per_page" . ,per_page) ("sort" . ,sort))))

(defun show-tag (&key tag_id)
    (http-get :path (concatenate 'string "/api/v2/tags/" tag_id)))

(defun show-user-following-tags (&key user_id (page 1) (per_page 20))
    (http-get :path (concatenate 'string "/api/v2/users/" user_id "/following_tags")
        :query `(("page" . ,page) ("per_page" . ,per_page))))

(defun delete-tag-following (&key tag_id token)
    (http-delete :path (concatenate 'string "/api/v2/tags/" tag_id "/following")
        :token token))

(defun get-tag-following (&key tag_id token)
    (http-get :path (concatenate 'string "/api/v2/tags/" tag_id "/following")
        :token token))

(defun put-tag-following (&key tag_id token)
    (http-put :path (concatenate 'string "/api/v2/tags/" tag_id "/following")
        :token token))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         user           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-item-stockers (&key item_id (page 1) (per_page 20))
    (http-get :path (concatenate 'string "/api/v2/items/" item_id "/stockers")
        :query `(("page" . ,page) ("per_page" . ,per_page))))

(defun get-users (&key (page 1) (per_page 20))
    (http-get :path (concatenate 'string "/api/v2/users")
        :query `(("page" . ,page) ("per_page" . ,per_page))))

(defun show-user (&key user_id)
    (http-get :path (concatenate 'string "/api/v2/users/" user_id)))

(defun show-user-followers (&key user_id)
    (http-get :path (concatenate 'string "/api/v2/users/" user_id "/followers")))

(defun delete-user-following (&key user_id token)
    (http-delete :path (concatenate 'string "/api/v2/users/" user_id "/following")
        :token token))

(defun get-user-following (&key user_id token)
    (http-get :path (concatenate 'string "/api/v2/users/" user_id "/following")
        :token token))

(defun put-user-following (&key user_id token)
    (http-put :path (concatenate 'string "/api/v2/users/" user_id "/following")
        :token token))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        items           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-authenticated-user-items (&key token (page 1) (per_page 20))
    (http-get :path "/api/v2/authenticated_user/items"
        :query `(("page" . ,page) ("per_page" . ,per_page))
        :token token))

(defun get-items (&key (page 1) (per_page 20) (query ""))
    (http-get :path "/api/v2/items"
        :query `(("page" . ,page) ("per_page" . ,per_page) ("query" . ,query))))


(defun post-item (&key token title body private name (versions '("1.0")))
    (let ((content (json:encode-json-to-string `((:title . ,title)
                                                    (:body . ,body)
                                                    (:private . ,private)
                                                    (:tags . (((:name . ,name) (:versions . ,versions))))))))
        (http-post :path "/api/v2/items"
            :content content
            :token token)))

(defun delete-item (&key token item_id)
    (http-delete :path (concatenate 'string "/api/v2/items/" item_id)
        :token token))

(defun show-item (&key item_id)
    (http-get :path (concatenate 'string "/api/v2/items/" item_id)))

(defun patch-item (&key token item_id title body private name (versions '("1.0")))
    (let ((content (json:encode-json-to-string `((:title . ,title)
                                                    (:body . ,body)
                                                    (:private . ,private)
                                                    (:tags . (((:name . ,name) (:versions . ,versions))))))))
        (http-patch :path (concatenate 'string "/api/v2/items/" item_id)
            :content content
            :token token)))

(defun put-item-stock (&key token item_id)
    (http-put :path (concatenate 'string "/api/v2/items/" item_id "/stock")
        :token token))

(defun get-tag-items (&key tag_id (page 1) (per_page 20))
    (http-get :path (concatenate 'string "/api/v2/tags/" tag_id "/items")
        :query `(("page" . ,page) ("per_page" . ,per_page))))

(defun get-user-stocks (&key user_id (page 1) (per_page 20))
    (http-get :path (concatenate 'string "/api/v2/users/" user_id "/stocks")
        :query `(("page" . ,page) ("per_page" . ,per_page))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    authentication      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-authenticated-user (&key token)
    (http-get :path "/api/v2/authenticated_user"
        :token token))
