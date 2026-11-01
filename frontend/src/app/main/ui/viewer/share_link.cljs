;; This Source Code Form is subject to the terms of the Mozilla Public
;; License, v. 2.0. If a copy of the MPL was not distributed with this
;; file, You can obtain one at http://mozilla.org/MPL/2.0/.
;;
;; Copyright (c) KALEIDOS SUBSIDIARY SL

(ns app.main.ui.viewer.share-link
  (:require-macros [app.main.style :as stl])
  (:require
   [app.common.data :as d]
   [app.common.data.macros :as dm]
   [app.common.logging :as log]
   [app.main.data.common :as dc]
   [app.main.data.event :as ev]
   [app.main.data.modal :as modal]
   [app.main.data.notifications :as ntf]
   [app.main.refs :as refs]
   [app.main.router :as rt]
   [app.main.store :as st]
   [app.main.ui.ds.buttons.button :refer [button*]]
   [app.main.ui.ds.buttons.icon-button :refer [icon-button*]]
   [app.main.ui.ds.controls.checkbox :refer [checkbox*]]
   [app.main.ui.ds.controls.input :refer [input*]]
   [app.main.ui.ds.controls.select :refer [select*]]
   [app.main.ui.ds.foundations.assets.icon :refer [icon*] :as i]
   [app.util.clipboard :as clipboard]
   [app.util.dom :as dom]
   [app.util.i18n :as i18n :refer [tr]]
   [cuerdas.core :as str]
   [rumext.v2 :as mf]))

(log/set-level! :warn)

(defn- prepare-params
  [{:keys [pages who-comment who-inspect]}]
  {:pages pages
   :who-comment who-comment
   :who-inspect who-inspect})

(defn- permission-label
  [value]
  (if (= "all" value)
    (tr "common.share-link.all-users")
    (tr "common.share-link.team-members")))

(defn- link->url
  [router route page-ids zoom-type link]
  (let [page-id (d/seek #(contains? (:pages link) %) page-ids)
        params  (-> (:query-params route)
                    (assoc :share-id (:id link))
                    (assoc :page-id page-id)
                    (assoc :index "0"))
        params  (if (nil? zoom-type)
                  (dissoc params :zoom)
                  (assoc params :zoom zoom-type))]
    (rt/resolve-uri router :viewer params)))

(mf/defc link-row*
  {::mf/private true
   ::mf/memo true}
  [{:keys [file link current pending-destroy link-url]}]
  (let [{:keys [pages who-comment who-inspect]} link
        page-names (dc/get-link-page-names file pages)
        url        (link-url link)]

    [:li {:class (stl/css-case :link-row true
                               :link-row-pending-destroy pending-destroy)}
     [:div {:class (stl/css :link-info)}
      [:div {:class (stl/css :link-url) :title url} url]

      [:div {:class (stl/css :link-line)}
       [:span {:class (stl/css :link-label)} (tr "common.share-link.permissions-pages")]
       [:span {:class (stl/css :link-value)} (if (str/empty? page-names)
                                               (tr "common.share-link.no-pages")
                                               page-names)]]

      [:div {:class (stl/css :link-line)}
       [:span {:class (stl/css :link-label)} (tr "common.share-link.permissions-can-comment")]
       [:span {:class (stl/css :link-value)} (permission-label who-comment)]]

      [:div {:class (stl/css :link-line)}
       [:span {:class (stl/css :link-label)} (tr "common.share-link.permissions-can-inspect")]
       [:span {:class (stl/css :link-value)} (permission-label who-inspect)]]]

     (when current
       [:span {:class (stl/css :current-tag)} (tr "common.share-link.current-tag")])]))

(mf/defc collapsible-panel*
  {::mf/private true}
  [{:keys [class title children]}]
  (let [expanded*  (mf/use-state false)
        expanded   (deref expanded*)

        content-id (mf/use-id)

        on-toggle
        (mf/use-fn
         #(swap! expanded* not))]

    [:div {:class class}
     [:button {:class (stl/css :share-link-collapsible-title)
               :type "button"
               :aria-expanded expanded
               :aria-controls content-id
               :on-click on-toggle}
      [:> icon* {:icon-id (if expanded i/arrow-down i/arrow-right)
                 :class (stl/css :share-link-collapsible-title-icon)}]
      title]

     (when ^boolean expanded
       children)]))

(mf/defc permissions-select-row*
  {::mf/private true}
  [{:keys [label value options on-change]}]
  [:div {:class (stl/css :permissions-select-row)}
   [:div {:class (stl/css :permissions-subtitle)} label]
   [:div {:class (stl/css :permissions-items)}
    [:> select* {:default-selected (dm/str value)
                 :options options
                 :has-portal true
                 :aria-label label
                 :on-change on-change}]]])

(mf/defc permissions-page-selection*
  {::mf/private true}
  [{:keys [file page selected on-toggle-all on-checked-page]}]
  (let [pages         (->> (get-in file [:data :pages])
                           (map #(get-in file [:data :pages-index %])))
        page-id       (:id page)
        all-selected? (= selected (into #{} (map :id) pages))]

    [:div {:class (stl/css :permissions-view-mode)}
     [:div {:class (stl/css :permissions-subtitle)}
      (tr "common.share-link.permissions-pages")]
     [:div {:class (stl/css :permissions-items)}
      (if (= 1 (count pages))
        [:div {:class (stl/css :permissions-page-row)}
         [:> checkbox* {:id (dm/str "page-" page-id)
                        :class (stl/css :permissions-checkbox)
                        :label (:name page)
                        :checked true
                        :on-change #(on-checked-page page-id %)}]
         [:span {:class (stl/css :permissions-current-tag)}
          (tr "common.share-link.current-tag")]]

        [:*
         [:div {:class (stl/css :permissions-select-all-row)}
          [:> checkbox* {:id "view-all"
                         :class (stl/css :permissions-checkbox)
                         :label (tr "common.share-link.view-all")
                         :checked all-selected?
                         :on-change on-toggle-all}]
          [:span {:class (stl/css :permissions-count-pages)}
           (tr "common.share-link.page-shared" (i18n/c (count selected)))]]

         [:ul {:class (stl/css :permissions-pages-selection)}
          (for [{:keys [id name]} pages]
            [:li {:class (stl/css :permissions-page-row)
                  :key (dm/str id)}
             [:> checkbox* {:id (dm/str "page-" id)
                            :class (stl/css :permissions-checkbox)
                            :label name
                            :checked (contains? selected id)
                            :on-change #(on-checked-page id %)}]
             (when (= page-id id)
               [:span {:class (stl/css :permissions-current-tag)}
                (tr "common.share-link.current-tag")])])]])]]))

(mf/defc permissions-section*
  {::mf/private true}
  [{:keys [file page options on-toggle-all on-checked-page
           on-comment-change on-inspect-change]}]
  (let [selected    (:pages options)
        permissions [{:id "team" :label (tr "common.share-link.team-members")}
                     {:id "all"  :label (tr "common.share-link.all-users")}]]

    [:> collapsible-panel* {:class (stl/css :permissions-section)
                            :title (tr "common.share-link.manage-ops")}
     [:div {:class (stl/css :permissions-content)}
      [:> permissions-page-selection* {:file file
                                       :page page
                                       :selected selected
                                       :on-toggle-all on-toggle-all
                                       :on-checked-page on-checked-page}]

      [:> permissions-select-row* {:label (tr "common.share-link.permissions-can-comment")
                                   :value (:who-comment options)
                                   :options permissions
                                   :on-change on-comment-change}]

      [:> permissions-select-row* {:label (tr "common.share-link.permissions-can-inspect")
                                   :value (:who-inspect options)
                                   :options permissions
                                   :on-change on-inspect-change}]]]))

(mf/defc links-section*
  {::mf/private true}
  [{:keys [file links current-id pending-destroy-id link-url]}]
  (when (seq links)
    [:> collapsible-panel* {:class (stl/css :links-section)
                            :title (dm/str (tr "common.share-link.links-title")
                                           " (" (count links) ")")}
     [:ul {:class (stl/css :links-list)}
      (for [{:keys [id] :as link} links]
        [:> link-row* {:key (dm/str id)
                       :file file
                       :link link
                       :current (= id current-id)
                       :pending-destroy (= id pending-destroy-id)
                       :link-url link-url}])]]))

(mf/defc share-link-dialog
  {::mf/register modal/components
   ::mf/register-as :share-link
   ::mf/wrap-props false}
  [{:keys [file page]}]
  (let [slinks          (mf/deref refs/share-links)
        router          (mf/deref refs/router)
        route           (mf/deref refs/route)
        zoom-type       (mf/deref refs/viewer-zoom-type)

        page-ids        (dm/get-in file [:data :pages])
        all-page-ids    (into #{} page-ids)

        dialog-ref      (mf/use-ref nil)

        ;; Link waiting for its destroy confirmation (nil when none).
        pending-destroy* (mf/use-state nil)
        pending-destroy  (deref pending-destroy*)

        options*        (mf/use-state
                         {:pages #{(:id page)}
                          :who-comment "team"
                          :who-inspect "team"})
        options         (deref options*)

        current-share
        (mf/with-memo [slinks options]
          (let [{:keys [pages who-comment who-inspect]} (prepare-params options)]
            (d/seek #(and (= (:who-inspect %) who-inspect)
                          (= (:who-comment %) who-comment)
                          (= (:pages %) pages))
                    slinks)))

        current-link
        (mf/with-memo [current-share page-ids route router zoom-type]
          (when (some? current-share)
            (link->url router route page-ids zoom-type current-share)))

        link-url
        (mf/use-fn
         (mf/deps router route page-ids zoom-type)
         (fn [link]
           (link->url router route page-ids zoom-type link)))

        on-close
        (mf/use-fn
         (fn [event]
           (dom/prevent-default event)
           (modal/hide!)))

        on-toggle-all
        (fn [_event]
          (swap! options*
                 (fn [{:keys [pages] :as state}]
                   (if (= pages all-page-ids)
                     (assoc state :pages #{(:id page)})
                     (assoc state :pages all-page-ids)))))

        on-checked-page
        (fn [page-id event]
          (let [checked? (dom/checked? (dom/get-target event))]
            (swap! options*
                   (fn [{:keys [pages] :as state}]
                     (let [pages (if checked?
                                   (conj pages page-id)
                                   (disj pages page-id))]
                       (if (seq pages)
                         (assoc state :pages pages)
                         state))))))

        on-create-link
        (fn [_]
          (let [params (prepare-params options)
                params (assoc params :file-id (:id file))]
            (st/emit! (dc/create-share-link params)
                      (ev/event {::ev/name "create-share-link"
                                 ::ev/origin "viewer"
                                 :can-comment (:who-comment params)
                                 :can-inspect-code (:who-inspect params)}))))

        on-copy-link
        (mf/use-fn
         (mf/deps current-link)
         (fn [_]
           (clipboard/to-clipboard current-link)
           (st/emit! (ntf/show {:level :info
                                :type :toast
                                :content (tr "common.share-link.link-copied-success")
                                :timeout 1000})
                     (ev/event {::ev/name "copy-share-link"
                                ::ev/origin "viewer"}))))

        on-destroy-current-link
        (mf/use-fn
         (mf/deps current-share)
         (fn [_]
           (reset! pending-destroy* current-share)))

        on-destroy-link
        (fn [_]
          (let [link pending-destroy]
            (reset! pending-destroy* nil)
            (st/emit! (dc/delete-share-link link))))

        on-cancel-destroy
        (fn [_]
          (reset! pending-destroy* nil))

        on-inspect-change
        (fn [value]
          (swap! options* assoc :who-inspect value))

        on-comment-change
        (fn [value]
          (swap! options* assoc :who-comment value))]

    (mf/with-effect []
      (let [previous (dom/get-active)]
        (some-> (mf/ref-val dialog-ref) dom/focus!)
        (fn []
          (when (and (some? previous) (.-isConnected ^js previous))
            (dom/focus! previous)))))

    [:div {:class (stl/css :share-link-modal)}
     [:div {:class (stl/css :share-link-dialog)
            :role "dialog"
            :aria-labelledby "share-link-title"
            :tab-index "-1"
            :ref dialog-ref}
      [:div {:class (stl/css :share-link-header)}
       [:h2 {:id "share-link-title"
             :class (stl/css :share-link-title)}
        (tr "common.share-link.title")]
       [:> icon-button* {:variant "ghost"
                         :class (stl/css :share-link-close-button)
                         :aria-label (tr "labels.close")
                         :icon i/close
                         :on-click on-close}]]

      [:div {:class (stl/css :share-link-content)}
       [:div {:class (stl/css :share-link-section)}
        (when (some? current-link)
          [:> input* {:type "text"
                      :class (stl/css :share-link-copy-input)
                      :value (or current-link "")
                      :placeholder (tr "common.share-link.placeholder")
                      :aria-labelledby "share-link-title"
                      :read-only true
                      :slot-end (mf/html
                                 [:> icon-button* {:variant "secondary"
                                                   :aria-label (tr "viewer.header.share.copy-link")
                                                   :icon i/clipboard
                                                   :on-click on-copy-link}])}])

        [:div {:class (stl/css :share-link-hint-wrapper)}
         [:div {:class (stl/css :share-link-hint)} (tr "common.share-link.permissions-hint")]
         (if (some? current-share)
           [:> button* {:variant "destructive"
                        :type "button"
                        :on-click on-destroy-current-link}
            (tr "common.share-link.destroy-link")]
           [:> button* {:variant "primary"
                        :type "button"
                        :on-click on-create-link}
            (tr "common.share-link.get-link")])]]

       (if (some? pending-destroy)
         [:div
          [:div {:class (stl/css :share-link-confirm-description)}
           (tr "common.share-link.confirm-deletion-link-description")]
          [:div {:class (stl/css :share-link-confirm-actions)}
           [:> button* {:variant "secondary"
                        :type "button"
                        :on-click on-cancel-destroy}
            (tr "labels.cancel")]
           [:> button* {:variant "destructive"
                        :type "button"
                        :on-click on-destroy-link}
            (tr "common.share-link.destroy-link")]]]

         [:> permissions-section* {:file file
                                   :page page
                                   :options options
                                   :on-toggle-all on-toggle-all
                                   :on-checked-page on-checked-page
                                   :on-comment-change on-comment-change
                                   :on-inspect-change on-inspect-change}])

       [:> links-section* {:file file
                           :links slinks
                           :current-id (:id current-share)
                           :pending-destroy-id (:id pending-destroy)
                           :link-url link-url}]]]]))
