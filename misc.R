library(methods)
library(RGtk2)
library(memisc)
#setwd("/Users/masahiro/Dropbox/Documents/R/Rz/Rz/R/")
setwd("/media/sf_Dropbox/Documents/R/Rz/Rz/R/")
sapply(dir(), source)
Rz()
#setwd("../../")


q(save="no")
#install.packages("/media/sf_Dropbox/Documents/R/Rz/Rz_0.3-0.tar.gz", repos=NULL)
library(RGtk2);library(Rz);Rz();data(iris);Rz()


bindtextdomain("Rz", "/home/masahiro/Documents/R/Rz/Rz/inst/po")
library(tools)
xgettext2pot("/home/masahiro/Documents/R/Rz/Rz",
             "/home/masahiro/Documents/R/Rz/Rz/inst/po/Rz.pot")
xgettext2pot("~/Dropbox/Documents/R/Rz/Rz",
             "~/Dropbox/Documents/R/Rz/Rz/inst/po/Rz.pot")

R CMD check Rz --no-manual --no-install
R CMD build Rz --binary
            
winMenuAdd("Rz")
winMenuAddItem("Rz", gettext("Start"), "Rz()")
            
install.packages("‾/Documents/R/Rz/Rz_0.2-3.zip", repos=NULL)


convertUnit(grid.locator()$x, "inches", valueOnly=TRUE)

testwin <- function(obj){
  w <- gtkWindowNew(show=FALSE)
  w$add(obj)
  w$showAll()
}

entry <- gtkEntryNew()
entry$SetProgressPulseStep(0.1)
testwin(entry)
entry$progressPulse()
entry$getActivatesDefault()

entry$setName("one")
entry2 <- gtkEntryNew()
entry3 <- gtkEntryNew()
entry$setIconFromStock(GtkEntryIconPosition["primary"], "gtk-find-and-replace")
entry2$setIconFromStock(GtkEntryIconPosition["primary"], "gtk-find")
entry3$setIconFromStock(GtkEntryIconPosition["primary"], "gtk-ok")
vbox <- gtkHBoxNew()
vbox$packStart(entry, expand=TRUE, fill=TRUE)
vbox$packStart(entry2, expand=TRUE, fill=TRUE)
vbox$packStart(entry3, expand=TRUE, fill=TRUE)
testwin(vbox)
vbox$show()
lapply(vbox$getChildren(), gtkWidgetShowAll)

entry$hideAll()

.Rz.path <- "/home/masahiro/Documents/R/Rz/Rz/inst"
catpix   <- gdkPixbufNewFromFile(paste(.Rz.path, "images/cat.png"  , sep="/"))$retval
orderpix <- gdkPixbufNewFromFile(paste(.Rz.path, "images/order.png", sep="/"))$retval
numpix   <- gdkPixbufNewFromFile(paste(.Rz.path, "images/num.png"  , sep="/"))$retval
errorpix <- gdkPixbufNewFromFile(paste(.Rz.path, "images/error.png", sep="/"))$retval
liststore2 <- gtkListStoreNew("GdkPixbuf")
for (i in list(catpix, orderpix, numpix)){
#  entry <- gtkEntryNew()
#  entry$setIconFromPixbuf(GtkEntryIconPosition["primary"], i)
  ## Add a new row to the model
  iter <- liststore2$append()$iter
  liststore2$set(iter, 0, i)
}
box <- gtkComboBoxNewWithModel(liststore2)
cp  <- gtkCellRendererPixbuf()
box$packStart(cp)
box$addAttribute(cp, "pixbuf", 0)
win <- gtkWindowNew(show=FALSE)
label <- gtkLabel("t4est")
win$add(label)
box$setTooltipWindow(win)
testwin(box)

entry <- gtkEntryNew()
win <- gtkWindowNew(show=FALSE)
label <- gtkLabel("t4est")
win$add(label)
win <- entry$setTooltipWindow(win)
testwin(entry)

i <- catpix   <- gdkPixbufNewFromFile(paste(.Rz.path, "images/cat.png"  , sep="/"))$retval
orderpix <- gdkPixbufNewFromFile(paste(.Rz.path, "images/order.png", sep="/"))$retval
numpix   <- gdkPixbufNewFromFile(paste(.Rz.path, "images/num.png"  , sep="/"))$retval
errorpix <- gdkPixbufNewFromFile(paste(.Rz.path, "images/error.png", sep="/"))$retval

for (i in list(catpix, orderpix, numpix, errorpix)){
  entry <- gtkEntryNew()
  entry$setIconFromPixbuf(GtkEntryIconPosition["primary"], i)
  ## Add a new row to the model
  iter <- liststore2$append()$iter
  liststore2$set(iter, 0, i)
}
box <- gtkComboBoxEntryNew()
cp <- gtkCellRendererPixbuf()
box$packStart(cp)
box$addAttribute(cp, "pixbuf", 0)
testwin(box)

liststore2 <- gtkListStoreNew("character","character","character")
sapply(list("a","b","c","d"),
       function(string) {
         iter <- liststore2$append()$iter
         liststore2$set(iter, 0, string, 1, "e", 2, string)
       })
box <- gtkComboBoxNewWithModel(liststore2)
#box["row-span-column"] <- 1
#box["column-span-column"] <- 2
cp <- gtkCellRendererText()
cq <- gtkCellRendererText()
box$packStart(cp, expand=FALSE)
box$packStart(cq)
box$addAttribute(cp, "text", 0)
box$addAttribute(cq, "text", 1)
testwin(box)

dialog <- gtkFileChooserDialogFilteredNew(title=gettext("Import"), parent=NULL, file.type.list=file.type.list)
hbox <- dialog$getContentArea()$getChildren()[[1]]$getChildren()[[1]]$getChildren()[[1]]$getChildren()[[3]]$getChildren()[[2]]$getChildren()[[2]]
combo <- gtkComboBoxNewText()
for(i in iconvlist()) combo$appendText(i)
index <- which(localeToCharset()==iconvlist()) - 1
if(length(index)==0) index <- -1
combo$setActive(index)
combo$setRowSpanColumn(0)
hbox$packEnd(combo, expand=FALSE)
#dialog$setExtraWidget(combo)
response <- dialog$run()

dialog <- gtkDialogNewWithButtons(title=gettext("Change the Name of Current Dataset"), parent=NULL, flags=c("modal", "destroy-with-parent"),
                                   "gtk-ok", GtkResponseType["ok"], 
                                   "gtk-cancel", GtkResponseType["cancel"],
                                   show=FALSE)
entry <- gtkEntryNew()
dialog$addActionWidget(entry, GtkResponseType["ok"])
#dialog[["vbox"]]$packStart(vbox1, expand = FALSE, padding = 2)


### todo
# log
# ラベル等をファイルから読み込み
# 複数の変数の一括操作
# スクリプトインターフェース
# レポート作成
# plot履歴
# 最近使ったファイル
# 画像のフォント
# 値ラベル変更
# 変数の計算
# ポップアップ改善
# 変数にタグ
# クロス集計表
# ワーディング管理
# rzdのエンコーディング
# ケースの選択
# 正規表現オプション

# !テーマが存在しないときの処理
# !data view
# !layer add
# !\の除去
# !欠損値，リコードのエラー処理
# !データを取り除くときの確認
# !同じデータセット名のチェック
# !変数名がvalidかどうか
# !ヒストグラム
# !層・凡例
# !ポップアップOFF
# !テーマセレクタ
# !変数ラベル出力
# !列の色
# !ggplotエラー等
# !csv等読み込み
# !cairo off
# !codebook off
# !plot
# !plotラベルまわり
# !出力
# !出力のヘッダーと入出力na.string
# !RzDataのバージョンアップ
