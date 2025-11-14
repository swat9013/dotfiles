
#!/bin/bash

#============================================================================
# OpenFortiVPN接続スクリプト
#============================================================================
#
# 【目的】
# FortiClient (wwwaveSSL-VPN) からOpenFortiVPNへの移行をサポートする
# インタラクティブな接続ラッパースクリプト
#
# 【使用方法】
# ./fortivpn.sh
# または aliasを使用: fvpn
#
# 【前提条件】
# - OpenFortiVPNのインストール (brew install openfortivpn)
# - sudoパスワードなし実行の設定（推奨、以下のコマンドを1回実行）:
#   echo "$(whoami) ALL=(ALL) NOPASSWD: /opt/homebrew/bin/openfortivpn" | sudo tee /etc/sudoers.d/openfortivpn
#   sudo chmod 440 /etc/sudoers.d/openfortivpn
#   ※この設定により、VPN接続時にパスワード入力が不要になります
#
# 【設定ディレクトリ構成】
# ~/.config/fortivpn/
# ├── config              # OpenFortiVPN設定ファイル
# └── certs/              # クライアント証明書格納ディレクトリ
#     ├── client.crt      # クライアント証明書 (オプション)
#     └── client.key      # クライアント秘密鍵 (オプション)
#
# 【設定ファイル (config)】
# パス: ~/.config/fortivpn/config
# 必須項目: host, port, username, password (または接続時に手動入力)
# 推奨項目: trusted-cert (サーバー証明書のSHA256フィンガープリント)
# オプション: user-cert, user-key (クライアント証明書のパス)
#
# 【機能】
# - 初回実行時の自動セットアップ (ディレクトリ・設定ファイル自動生成)
# - 設定ファイルの存在確認と検証
# - 未設定項目の警告表示
# - OpenFortiVPNのインストール確認
# - 接続前の対話的確認プロンプト
# - レガシーTLS対応 (SHA-1証明書サポート)
#
# 【終了方法】
# Ctrl+C で接続を終了
#
#============================================================================

# 設定ファイルとディレクトリのパス
CONFIG_DIR="$HOME/.config/fortivpn"
CONFIG_FILE="$CONFIG_DIR/config"
CERTS_DIR="$CONFIG_DIR/certs"

# 色の定義
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# 初回セットアップ関数
setup_initial_config() {
    echo "==========================================="
    echo "OpenFortiVPN 初回セットアップ"
    echo "==========================================="
    echo ""
    printf "${BLUE}設定ディレクトリが見つかりません。${NC}\n"
    echo "自動的にセットアップを行います..."
    echo ""

    # ディレクトリの作成
    printf "${GREEN}[1/3]${NC} ディレクトリを作成中...\n"
    mkdir -p "$CONFIG_DIR"
    mkdir -p "$CERTS_DIR"
    echo "  ✓ $CONFIG_DIR"
    echo "  ✓ $CERTS_DIR"
    echo ""

    # サンプル設定ファイルの生成
    printf "${GREEN}[2/3]${NC} サンプル設定ファイルを生成中...\n"
    cat > "$CONFIG_FILE" << EOF
# OpenFortiVPN 設定ファイル
# 詳細: man openfortivpn

# === 必須設定 ===
# VPNサーバーのホスト名またはIPアドレス
host = YOUR_VPN_HOST_HERE
# VPNサーバーのポート番号 (通常は443または10443)
port = 443

# === 認証情報 ===
# ユーザー名 (YOUR_USERNAME_HEREを実際のユーザー名に置き換えてください)
username = YOUR_USERNAME_HERE
# パスワード (YOUR_PASSWORD_HEREを実際のパスワードに置き換えるか、コメントアウトして接続時に入力)
password = YOUR_PASSWORD_HERE

# === サーバー証明書 (推奨) ===
# 初回接続時に表示されるSHA256フィンガープリントをここに設定
# trusted-cert = 1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef

# === クライアント証明書 (オプション) ===
# クライアント証明書が必要な場合は、以下のコメントを外して設定
# 注意: 絶対パスで指定してください
# user-cert = $HOME/.config/fortivpn/certs/client.crt
# user-key = $HOME/.config/fortivpn/certs/client.key
EOF
    echo "  ✓ $CONFIG_FILE"
    echo ""

    # 設定手順の案内
    printf "${GREEN}[3/3]${NC} セットアップ完了！\n"
    echo ""
    echo "==========================================="
    printf "${YELLOW}次のステップ:${NC}\n"
    echo "==========================================="
    echo ""
    echo "1. 設定ファイルを編集してください:"
    printf "   ${BLUE}vi $CONFIG_FILE${NC}\n"
    echo ""
    echo "   必須: 以下のプレースホルダーを実際の値に置き換えてください"
    echo "   - YOUR_VPN_HOST_HERE → VPNサーバーのホスト名"
    echo "   - YOUR_USERNAME_HERE → あなたのユーザー名"
    echo "   - YOUR_PASSWORD_HERE → あなたのパスワード"
    echo ""
    echo "2. (オプション) クライアント証明書が必要な場合:"
    echo "   証明書ファイルを以下に配置してください"
    printf "   ${BLUE}$CERTS_DIR/${NC}\n"
    echo ""
    echo "3. 設定完了後、再度このスクリプトを実行してください:"
    printf "   ${BLUE}fvpn${NC}\n"
    echo ""
    exit 0
}

# 初回セットアップチェック
if [ ! -d "$CONFIG_DIR" ]; then
    setup_initial_config
fi

echo "==========================================="
echo "OpenFortiVPN 接続スクリプト"
echo "==========================================="
echo ""

# 設定ファイルの存在確認
if [ ! -f "$CONFIG_FILE" ]; then
    printf "${RED}エラー: 設定ファイルが見つかりません: $CONFIG_FILE${NC}\n"
    echo ""
    echo "設定ディレクトリは存在しますが、設定ファイルがありません。"
    echo "初回セットアップを実行してください:"
    echo "  rm -rf $CONFIG_DIR"
    echo "  fvpn"
    echo ""
    exit 1
fi

# 設定ファイルの確認
echo "設定ファイル: $CONFIG_FILE"
echo ""
echo "--- 設定内容 ---"
grep -E "^(host|port|username)" "$CONFIG_FILE" | sed 's/^/  /'
echo ""

# ユーザー名とパスワードの確認
if grep -qE "^username\s*=\s*YOUR_USERNAME_HERE" "$CONFIG_FILE"; then
    printf "${YELLOW}警告: ユーザー名が設定されていません${NC}\n"
    echo "  設定ファイルの 'YOUR_USERNAME_HERE' を実際のユーザー名に置き換えてください"
    echo ""
fi

if grep -qE "^password\s*=\s*YOUR_PASSWORD_HERE" "$CONFIG_FILE"; then
    printf "${YELLOW}警告: パスワードが設定されていません${NC}\n"
    echo "  設定ファイルの 'YOUR_PASSWORD_HERE' を実際のパスワードに置き換えてください"
    echo "  または、接続時に手動で入力します"
    echo ""
fi

# trusted-certの確認
if ! grep -q "^trusted-cert" "$CONFIG_FILE"; then
    printf "${YELLOW}注意: サーバー証明書が未設定です${NC}\n"
    echo "  初回接続時に証明書エラーが表示されます"
    echo "  表示されたSHA256フィンガープリントを設定ファイルに追加してください"
    echo ""
fi

# クライアント証明書の確認
if grep -q "^# user-cert" "$CONFIG_FILE"; then
    printf "${YELLOW}注意: クライアント証明書が未設定です${NC}\n"
    echo "  必要な場合は、以下の手順で設定してください："
    echo "  1. macOSキーチェーンから証明書をエクスポート (.p12形式)"
    echo "  2. PEM形式に変換"
    echo "  3. 設定ファイルのuser-cert/user-keyの行を有効化してパスを指定"
    echo ""
fi

# openfortivpnのインストール確認
if ! command -v openfortivpn &> /dev/null; then
    printf "${RED}エラー: openfortivpnがインストールされていません${NC}\n"
    echo ""
    echo "インストール方法 (Homebrew):"
    echo "  brew install openfortivpn"
    echo ""
    exit 1
fi

echo "openfortivpnバージョン: $(openfortivpn --version 2>&1 | head -1)"
echo ""
echo ""
printf "${GREEN}VPN接続を開始します...${NC}\n"
echo "終了するには Ctrl+C を押してください"
echo ""

# VPN接続を開始（sudoが必要）
# cipher-listオプションを使用してSHA-1証明書に対応
sudo openfortivpn -c "$CONFIG_FILE" --cipher-list=DEFAULT:@SECLEVEL=0 --min-tls=1.0

# 接続終了後
echo ""
printf "${YELLOW}VPN接続が終了しました${NC}\n"
