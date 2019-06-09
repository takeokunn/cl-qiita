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
        ;; comments
        :delete-comment
        :show-comment
        :patch-comment
        :get-item-comments
        :post-item-comment
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
        :show-user-followees
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
        :put-user-stock
        :delete-user-stock
        :show-item-stock
        :get-user-items
        :get-tag-items
        :get-user-stocks
        ;; authentication
        :get-authenticated-user))
(in-package :cl-qiita)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        likes           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-likes (&key item-id)
    "url: https://qiita.com/api/v2/docs#get-apiv2itemsitem_idlikes"
    (declare (type string item-id))
    (http-get :path (concatenate 'string "/api/v2/items/" item-id "/likes")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        comment         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun delete-comment (&key comment-id token)
    "url: https://qiita.com/api/v2/docs#delete-apiv2commentscomment_id"
    (declare (type string comment-id token))
    (http-delete :path (concatenate 'string "/api/v2/comments/" comment-id)
        :token token))

(defun show-comment (&key comment-id)
    "url: https://qiita.com/api/v2/docs#get-apiv2commentscomment_id"
    (declare (type string comment-id))
    (http-get :path (concatenate 'string "/api/v2/comments/" comment-id)))

(defun patch-comment (&key comment-id body token)
    "url: https://qiita.com/api/v2/docs#patch-apiv2commentscomment_id"
    (declare (type string comment-id body token))
    (let ((content (json:encode-json-to-string `((:body . ,body)))))
        (http-patch :path (concatenate 'string "/api/v2/comments/" comment-id)
            :content content
            :token token)))

(defun get-item-comments (&key item-id)
    "url: https://qiita.com/api/v2/docs#get-apiv2itemsitem_idcomments"
    (declare (type string item-id))
    (http-get :path (concatenate 'string "/api/v2/items/" item-id "/comments")))

(defun post-item-comment (&key item-id body token)
    "url: https://qiita.com/api/v2/docs#post-apiv2itemsitem_idcomments"
    (declare (type string item-id))
    (let ((content (json:encode-json-to-string `((:body . ,body)))))
        (http-post :path (concatenate 'string "/api/v2/items/" item-id "/comments")
            :content content
            :token token)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         tags           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-tags (&key (page 1) (per-page 20) (sort "count"))
    "url: https://qiita.com/api/v2/docs#get-apiv2tags"
    (declare (type fixnum page per-page) (type string sort))
    (http-get :path "/api/v2/tags"
        :query `(("page" . ,page) ("per_page" . ,per-page) ("sort" . ,sort))))

(defun show-tag (&key tag-id)
    "url: https://qiita.com/api/v2/docs#get-apiv2tagstag_id"
    (declare (type string tag-id))
    (http-get :path (concatenate 'string "/api/v2/tags/" tag-id)))

(defun show-user-following-tags (&key user-id (page 1) (per-page 20))
    "url: https://qiita.com/api/v2/docs#get-apiv2usersuser_idfollowing_tags"
    (declare (type string user-id) (type fixnum page per-page))
    (http-get :path (concatenate 'string "/api/v2/users/" user-id "/following_tags")
        :query `(("page" . ,page) ("per_page" . ,per-page))))

(defun delete-tag-following (&key tag-id token)
    "url: https://qiita.com/api/v2/docs#delete-apiv2tagstag_idfollowing"
    (declare (type string tag-id token))
    (http-delete :path (concatenate 'string "/api/v2/tags/" tag-id "/following")
        :token token))

(defun get-tag-following (&key tag-id token)
    "url: https://qiita.com/api/v2/docs#get-apiv2tagstag_idfollowing"
    (declare (type string tag-id token))
    (http-get :path (concatenate 'string "/api/v2/tags/" tag-id "/following")
        :token token))

(defun put-tag-following (&key tag-id token)
    "url: https://qiita.com/api/v2/docs#put-apiv2tagstag_idfollowing"
    (declare (type string tag-id token))
    (http-put :path (concatenate 'string "/api/v2/tags/" tag-id "/following")
        :token token))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         user           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-item-stockers (&key item-id (page 1) (per-page 20))
    "url: https://qiita.com/api/v2/docs#get-apiv2itemsitem_idstockers"
    (declare (type string item-id) (type fixnum page per-page))
    (http-get :path (concatenate 'string "/api/v2/items/" item-id "/stockers")
        :query `(("page" . ,page) ("per_page" . ,per-page))))

(defun get-users (&key (page 1) (per-page 20))
    "url: https://qiita.com/api/v2/docs#get-apiv2users"
    (declare (type fixnum page per-page))
    (http-get :path (concatenate 'string "/api/v2/users")
        :query `(("page" . ,page) ("per_page" . ,per-page))))

(defun show-user (&key user-id)
    "url: https://qiita.com/api/v2/docs#get-apiv2usersuser_id"
    (declare (type string user-id))
    (http-get :path (concatenate 'string "/api/v2/users/" user-id)))

(defun show-user-followees (&key user-id (page 1) (per-page 20))
    "url: https://qiita.com/api/v2/docs#get-apiv2usersuser_idfollowees"
    (declare (type string user-id) (type fixnum page per_page))
    (http-get :path (concatenate 'string "/api/v2/users/" user-id "/followees")
        :query `(("page" . ,page) ("per_page" . ,per-page))))

(defun show-user-followers (&key user-id (page 1) (per-page 20))
    "url: https://qiita.com/api/v2/docs#get-apiv2usersuser_idfollowers"
    (declare (type string user-id) (type fixnum page per-page))
    (http-get :path (concatenate 'string "/api/v2/users/" user-id "/followers")
        :query `(("page" . ,page) ("per_page" . ,per-page))))

(defun delete-user-following (&key user-id token)
    "url: https://qiita.com/api/v2/docs#delete-apiv2usersuser_idfollowing"
    (declare (type string user-id token))
    (http-delete :path (concatenate 'string "/api/v2/users/" user-id "/following")
        :token token))

(defun get-user-following (&key user-id token)
    "url: https://qiita.com/api/v2/docs#get-apiv2usersuser_idfollowing"
    (declare (type string user-id token))
    (http-get :path (concatenate 'string "/api/v2/users/" user-id "/following")
        :token token))

(defun put-user-following (&key user-id token)
    "url: https://qiita.com/api/v2/docs#put-apiv2usersuser_idfollowing"
    (declare (type string user-id token))
    (http-put :path (concatenate 'string "/api/v2/users/" user-id "/following")
        :token token))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        items           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-authenticated-user-items (&key token (page 1) (per-page 20))
    "url: https://qiita.com/api/v2/docs#get-apiv2authenticated_useritems"
    (declare (type string token) (type fixnum page per-page))
    (http-get :path "/api/v2/authenticated_user/items"
        :query `(("page" . ,page) ("per_page" . ,per-page))
        :token token))

(defun get-items (&key (page 1) (per-page 20) (query ""))
    "url: https://qiita.com/api/v2/docs#get-apiv2items"
    (declare (type fixnum page per-page) (type string query))
    (http-get :path "/api/v2/items"
        :query `(("page" . ,page) ("per_page" . ,per-page) ("query" . ,query))))


(defun post-item (&key token title body private name (versions '("1.0")))
    "url: https://qiita.com/api/v2/docs#post-apiv2items"
    (let ((content (json:encode-json-to-string `((:title . ,title)
                                                    (:body . ,body)
                                                    (:private . ,private)
                                                    (:tags . (((:name . ,name) (:versions . ,versions))))))))
        (http-post :path "/api/v2/items"
            :content content
            :token token)))

(defun delete-item (&key token item-id)
    "url: https://qiita.com/api/v2/docs#delete-apiv2itemsitem_id"
    (declare (type string token item-id))
    (http-delete :path (concatenate 'string "/api/v2/items/" item-id)
        :token token))

(defun show-item (&key item-id)
    "url: https://qiita.com/api/v2/docs#get-apiv2itemsitem_id"
    (declare (type string item-id))
    (http-get :path (concatenate 'string "/api/v2/items/" item-id)))

(defun patch-item (&key token item-id title body private name (versions '("1.0")))
    "url: https://qiita.com/api/v2/docs#patch-apiv2itemsitem_id"
    (let ((content (json:encode-json-to-string `((:title . ,title)
                                                    (:body . ,body)
                                                    (:private . ,private)
                                                    (:tags . (((:name . ,name) (:versions . ,versions))))))))
        (http-patch :path (concatenate 'string "/api/v2/items/" item-id)
            :content content
            :token token)))

(defun put-item-stock (&key token item-id)
    "url: https://qiita.com/api/v2/docs#put-apiv2itemsitem_idstock"
    (declare (type string token item-id))
    (http-put :path (concatenate 'string "/api/v2/items/" item-id "/stock")
        :token token))

(defun put-user-stock (&key token item-id)
    "url: https://qiita.com/api/v2/docs#put-apiv2itemsitem_idstock"
    (declare (type string token item-id))
    (http-put :path (concatenate 'string "/api/v2/items/" item-id "/stock")
        :token token))

(defun delete-user-stock (&key token item-id)
    "url: https://qiita.com/api/v2/docs#delete-apiv2itemsitem_idstock"
    (declare (type string token item-id))
    (http-delete :path (concatenate 'string "/api/v2/items/" item-id "/stock")
        :token token))

(defun show-item-stock (&key token item-id)
    "url: https://qiita.com/api/v2/docs#get-apiv2itemsitem_idstock"
    (http-get :path (concatenate 'string "/api/v2/items/" item-id "/stock")
        :token token))

(defun get-user-stocks (&key user-id (page 1) (per-page 20))
    "url: https://qiita.com/api/v2/docs#get-apiv2itemsitem_idstock"
    (declare (type string user_id) (type fixnum page per-page))
    (http-get :path (concatenate 'string "/api/v2/users/" user-id "/stocks")
        :query `(("page" . ,page) ("per_page" . ,per-page))))

(defun get-tag-items (&key tag-id (page 1) (per-page 20))
    "url: https://qiita.com/api/v2/docs#delete-apiv2itemsitem_idstock"
    (declare (type string tag-id) (type fixnum page per-page))
    (http-get :path (concatenate 'string "/api/v2/tags/" tag-id "/items")
        :query `(("page" . ,page) ("per_page" . ,per-page))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    authentication      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-authenticated-user (&key token)
    "url: https://qiita.com/api/v2/docs#get-apiv2authenticated_user"
    (declare (type string token))
    (http-get :path "/api/v2/authenticated_user"
        :token token))
