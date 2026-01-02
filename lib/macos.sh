#!/bin/sh
# macOS固有の設定

# macOS以外では実行しない
if [ "$(uname)" != 'Darwin' ]; then
    exit 0
fi

echo "Configuring macOS settings..."

# 入力ソース切り替えショートカットを無効化
# ID 60: 前の入力ソースを選択 (Ctrl+Space)
# ID 61: 次の入力ソースを選択 (Ctrl+Option+Space)
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 60 '<dict><key>enabled</key><false/><key>value</key><dict><key>parameters</key><array><integer>32</integer><integer>49</integer><integer>262144</integer></array><key>type</key><string>standard</string></dict></dict>'
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 61 '<dict><key>enabled</key><false/><key>value</key><dict><key>parameters</key><array><integer>32</integer><integer>49</integer><integer>786432</integer></array><key>type</key><string>standard</string></dict></dict>'

# 設定を反映
/System/Library/PrivateFrameworks/SystemAdministration.framework/Resources/activateSettings -u

echo "Done. Input source shortcuts (Ctrl+Space) have been disabled."
