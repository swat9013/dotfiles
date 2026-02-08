#!/bin/sh
# Ghostty macOS defaults設定
#
# macOSはCtrl+Tab/Ctrl+Shift+Tabを「Show Next/Previous Tab」として
# アプリに到達する前にインターセプトする。
# 到達不可能なキーに割り当てて実質無効化し、Zellijにキーを転送可能にする。
#
# 元に戻す場合:
#   defaults delete com.mitchellh.ghostty NSUserKeyEquivalents

case "$(uname)" in
    Darwin)
        defaults write com.mitchellh.ghostty NSUserKeyEquivalents '{
            "Show Next Tab" = "@~^\\$F19";
            "Show Previous Tab" = "@~^\\$F18";
        }'
        echo "Ghostty: macOS defaults applied"
        ;;
    *)
        echo "Ghostty: skipping macOS defaults (not macOS)"
        ;;
esac
