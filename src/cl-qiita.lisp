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
        :get-tag-items
        :get-user-stocks
        ;; authentication
        :get-authenticated-user))
(in-package :cl-qiita)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        likes           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-likes (&key item_id)
    "url: https://qiita.com/api/v2/docs#get-apiv2itemsitem_idlikes"
    (declare (type string item_id))
    (http-get :path (concatenate 'string "/api/v2/items/" item_id "/likes")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        comment         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun delete-comment (&key comment_id token)
    "url: https://qiita.com/api/v2/docs#delete-apiv2commentscomment_id"
    (declare (type string comment_id token))
    (http-delete :path (concatenate 'string "/api/v2/comments/" comment_id)
        :token token))

(defun show-comment (&key comment_id)
    "url: https://qiita.com/api/v2/docs#get-apiv2commentscomment_id"
    (declare (type string comment_id))
    (http-get :path (concatenate 'string "/api/v2/comments/" comment_id)))

(defun patch-comment (&key comment_id body token)
    "url: https://qiita.com/api/v2/docs#patch-apiv2commentscomment_id"
    (declare (type string comment_id body token))
    (let ((content (json:encode-json-to-string `((:body . ,body)))))
        (http-patch :path (concatenate 'string "/api/v2/comments/" comment_id)
            :content content
            :token token)))

(defun get-item-comments (&key item_id)
    "url: https://qiita.com/api/v2/docs#get-apiv2itemsitem_idcomments"
    (declare (type string item_id))
    (http-get :path (concatenate 'string "/api/v2/items/" item_id "/comments")))

(defun post-item-comment (&key item_id body token)
    "url: https://qiita.com/api/v2/docs#post-apiv2itemsitem_idcomments"
    (declare (type string item_id))
    (let ((content (json:encode-json-to-string `((:body . ,body)))))
        (http-post :path (concatenate 'string "/api/v2/items/" item_id "/comments")
            :content content
            :token token)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         tags           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-tags (&key (page 1) (per_page 20) (sort "count"))
    "url: https://qiita.com/api/v2/docs#get-apiv2tags"
    (declare (type fixnum page per_page) (type string sort))
    (http-get :path "/api/v2/tags"
        :query `(("page" . ,page) ("per_page" . ,per_page) ("sort" . ,sort))))

(defun show-tag (&key tag_id)
    "url: https://qiita.com/api/v2/docs#get-apiv2tagstag_id"
    (declare (type string tag_id))
    (http-get :path (concatenate 'string "/api/v2/tags/" tag_id)))

(defun show-user-following-tags (&key user_id (page 1) (per_page 20))
    "url: https://qiita.com/api/v2/docs#get-apiv2usersuser_idfollowing_tags"
    (declare (type string user_id) (type fixnum page per_page))
    (http-get :path (concatenate 'string "/api/v2/users/" user_id "/following_tags")
        :query `(("page" . ,page) ("per_page" . ,per_page))))

(defun delete-tag-following (&key tag_id token)
    "url: https://qiita.com/api/v2/docs#delete-apiv2tagstag_idfollowing"
    (declare (type string tag_id token))
    (http-delete :path (concatenate 'string "/api/v2/tags/" tag_id "/following")
        :token token))

(defun get-tag-following (&key tag_id token)
    "url: https://qiita.com/api/v2/docs#get-apiv2tagstag_idfollowing"
    (declare (type string tag_id token))
    (http-get :path (concatenate 'string "/api/v2/tags/" tag_id "/following")
        :token token))

(defun put-tag-following (&key tag_id token)
    "url: https://qiita.com/api/v2/docs#put-apiv2tagstag_idfollowing"
    (declare (type string tag_id token))
    (http-put :path (concatenate 'string "/api/v2/tags/" tag_id "/following")
        :token token))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;         user           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-item-stockers (&key item_id (page 1) (per_page 20))
    "url: https://qiita.com/api/v2/docs#get-apiv2itemsitem_idstockers"
    (declare (type string item_id) (type fixnum page per_page))
    (http-get :path (concatenate 'string "/api/v2/items/" item_id "/stockers")
        :query `(("page" . ,page) ("per_page" . ,per_page))))

(defun get-users (&key (page 1) (per_page 20))
    "url: https://qiita.com/api/v2/docs#get-apiv2users"
    (declare (type fixnum page per_page))
    (http-get :path (concatenate 'string "/api/v2/users")
        :query `(("page" . ,page) ("per_page" . ,per_page))))

(defun show-user (&key user_id)
    "url: https://qiita.com/api/v2/docs#get-apiv2usersuser_id"
    (declare (type string user_id))
    (http-get :path (concatenate 'string "/api/v2/users/" user_id)))

(defun show-user-followees (&key user_id (page 1) (per_page 20))
    "url: https://qiita.com/api/v2/docs#get-apiv2usersuser_idfollowees"
    (declare (type string user_id) (type fixnum page per_page))
    (http-get :path (concatenate 'string "/api/v2/users/" user_id "/followees")
        :query `(("page" . ,page) ("per_page" . ,per_page))))

(defun show-user-followers (&key user_id (page 1) (per_page 20))
    "url: https://qiita.com/api/v2/docs#get-apiv2usersuser_idfollowers"
    (declare (type string user_id) (type fixnum page per_page))
    (http-get :path (concatenate 'string "/api/v2/users/" user_id "/followers")
        :query `(("page" . ,page) ("per_page" . ,per_page))))

(defun delete-user-following (&key user_id token)
    "url: https://qiita.com/api/v2/docs#delete-apiv2usersuser_idfollowing"
    (declare (type string user_id token))
    (http-delete :path (concatenate 'string "/api/v2/users/" user_id "/following")
        :token token))

(defun get-user-following (&key user_id token)
    "url: https://qiita.com/api/v2/docs#get-apiv2usersuser_idfollowing"
    (declare (type string user_id token))
    (http-get :path (concatenate 'string "/api/v2/users/" user_id "/following")
        :token token))

(defun put-user-following (&key user_id token)
    "url: https://qiita.com/api/v2/docs#put-apiv2usersuser_idfollowing"
    (declare (type string user_id token))
    (http-put :path (concatenate 'string "/api/v2/users/" user_id "/following")
        :token token))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;        items           ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-authenticated-user-items (&key token (page 1) (per_page 20))
    "url: https://qiita.com/api/v2/docs#get-apiv2authenticated_useritems"
    (declare (type string token) (type fixnum page per_page))
    (http-get :path "/api/v2/authenticated_user/items"
        :query `(("page" . ,page) ("per_page" . ,per_page))
        :token token))

(defun get-items (&key (page 1) (per_page 20) (query ""))
    "url: https://qiita.com/api/v2/docs#get-apiv2items"
    (declare (type fixnum page per_page) (type string query))
    (http-get :path "/api/v2/items"
        :query `(("page" . ,page) ("per_page" . ,per_page) ("query" . ,query))))


(defun post-item (&key token title body private name (versions '("1.0")))
    "url: https://qiita.com/api/v2/docs#post-apiv2items"
    (let ((content (json:encode-json-to-string `((:title . ,title)
                                                    (:body . ,body)
                                                    (:private . ,private)
                                                    (:tags . (((:name . ,name) (:versions . ,versions))))))))
        (http-post :path "/api/v2/items"
            :content content
            :token token)))

(defun delete-item (&key token item_id)
    "url: https://qiita.com/api/v2/docs#delete-apiv2itemsitem_id"
    (declare (type string token item_id))
    (http-delete :path (concatenate 'string "/api/v2/items/" item_id)
        :token token))

(defun show-item (&key item_id)
    "url: https://qiita.com/api/v2/docs#get-apiv2itemsitem_id"
    (declare (type string item_id))
    (http-get :path (concatenate 'string "/api/v2/items/" item_id)))

(defun patch-item (&key token item_id title body private name (versions '("1.0")))
    "url: https://qiita.com/api/v2/docs#patch-apiv2itemsitem_id"
    (let ((content (json:encode-json-to-string `((:title . ,title)
                                                    (:body . ,body)
                                                    (:private . ,private)
                                                    (:tags . (((:name . ,name) (:versions . ,versions))))))))
        (http-patch :path (concatenate 'string "/api/v2/items/" item_id)
            :content content
            :token token)))

(defun put-item-stock (&key token item_id)
    "url: https://qiita.com/api/v2/docs#put-apiv2itemsitem_idstock"
    (declare (type string token item_id))
    (http-put :path (concatenate 'string "/api/v2/items/" item_id "/stock")
        :token token))

(defun put-user-stock ()
    "url: https://qiita.com/api/v2/docs#put-apiv2itemsitem_idstock"
    ())

(defun delete-user-stock ()
    "url: https://qiita.com/api/v2/docs#delete-apiv2itemsitem_idstock"
    ())

(defun show-item-stock ()
    "url: https://qiita.com/api/v2/docs#get-apiv2itemsitem_idstock"
    ())

(defun get-user-items ()
    "url: https://qiita.com/api/v2/docs#get-apiv2usersuser_iditems"
    ())

(defun get-user-stocks (&key user_id (page 1) (per_page 20))
    "url: https://qiita.com/api/v2/docs#get-apiv2itemsitem_idstock"
    (declare (type string user_id) (type fixnum page per_page))
    (http-get :path (concatenate 'string "/api/v2/users/" user_id "/stocks")
        :query `(("page" . ,page) ("per_page" . ,per_page))))

(defun get-tag-items (&key tag_id (page 1) (per_page 20))
    "url: https://qiita.com/api/v2/docs#delete-apiv2itemsitem_idstock"
    (declare (type string tag_id) (type fixnum page per_page))
    (http-get :path (concatenate 'string "/api/v2/tags/" tag_id "/items")
        :query `(("page" . ,page) ("per_page" . ,per_page))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    authentication      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-authenticated-user (&key token)
    "url: https://qiita.com/api/v2/docs#get-apiv2authenticated_user"
    (declare (type string token))
    (http-get :path "/api/v2/authenticated_user"
        :token token))
