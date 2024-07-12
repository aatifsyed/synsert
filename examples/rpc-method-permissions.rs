use clap::Parser;
use quote::ToTokens;
use std::path::PathBuf;
use syn::visit::Visit;
use synsert::Editor;

#[derive(Parser)]
struct Args {
    #[arg(num_args(1..), required = true)]
    file: Vec<PathBuf>,
}

fn main() {
    let Args { file } = Args::parse();
    synsert::harness::run(
        file,
        |_, editor| Some(Visitor { editor }),
        |Visitor { editor }| Some(editor),
    )
}

struct Visitor {
    editor: Editor,
}

impl Visit<'_> for Visitor {
    fn visit_item_impl(&mut self, i: &'_ syn::ItemImpl) {
        if let Some((_, it, _)) = i.trait_.as_ref() {
            if it.segments.last().is_some_and(|it| it.ident == "RpcMethod") {
                let needle = i.self_ty.to_token_stream().to_string();
                if let Some((_, perm)) = METHOD_NAME2PERMISSION
                    .iter()
                    .find(|(name, _)| *name == needle.trim())
                {
                    if let Some(last) = i
                        .items
                        .iter()
                        .filter(|it| matches!(it, syn::ImplItem::Const(_)))
                        .last()
                    {
                        self.editor
                            .append(last, format!("const PERMISSION: Permission = {};", perm));
                    }
                }
            }
        }
    }
}

const METHOD_NAME2PERMISSION: &[(&str, &str)] = &[
    ("ChainExport", "Permission::Read"),
    ("ChainGetBlock", "Permission::Read"),
    ("ChainGetBlockMessages", "Permission::Read"),
    ("ChainGetGenesis", "Permission::Read"),
    ("ChainGetMessage", "Permission::Read"),
    ("ChainGetMessagesInTipset", "Permission::Read"),
    ("ChainGetMinBaseFee", "Permission::Admin"),
    ("ChainGetParentMessages", "Permission::Read"),
    ("ChainGetParentReceipts", "Permission::Read"),
    ("ChainGetPath", "Permission::Read"),
    ("ChainGetTipSet", "Permission::Read"),
    ("ChainGetTipSetAfterHeight", "Permission::Read"),
    ("ChainGetTipSetByHeight", "Permission::Read"),
    ("ChainHasObj", "Permission::Read"),
    ("ChainHead", "Permission::Read"),
    ("ChainReadObj", "Permission::Read"),
    ("ChainSetHead", "Permission::Admin"),
    ("ChainTipSetWeight", "Permission::Read"),
    ("EthAccounts", "Permission::Read"),
    ("EthBlockNumber", "Permission::Read"),
    ("EthChainId", "Permission::Read"),
    ("EthGasPrice", "Permission::Read"),
    ("EthGetBalance", "Permission::Read"),
    ("EthGetBlockByNumber", "Permission::Read"),
    ("EthSyncing", "Permission::Read"),
    ("GasEstimateFeeCap", "Permission::Read"),
    ("GasEstimateGasLimit", "Permission::Read"),
    ("GasEstimateGasPremium", "Permission::Read"),
    ("GasEstimateMessageGas", "Permission::Read"),
    ("MinerGetBaseInfo", "Permission::Read"),
    ("MpoolGetNonce", "Permission::Read"),
    ("MpoolPending", "Permission::Read"),
    ("MpoolPush", "Permission::Read"),
    ("MpoolPushMessage", "Permission::Sign"),
    ("MpoolSelect", "Permission::Read"),
    ("MsigGetAvailableBalance", "Permission::Read"),
    ("MsigGetPending", "Permission::Read"),
    ("NetAddrsListen", "Permission::Read"),
    ("NetAgentVersion", "Permission::Read"),
    ("NetAutoNatStatus", "Permission::Read"),
    ("NetConnect", "Permission::Write"),
    ("NetDisconnect", "Permission::Write"),
    ("NetInfo", "Permission::Read"),
    ("NetListening", "Permission::Read"),
    ("NetPeers", "Permission::Read"),
    ("NetVersion", "Permission::Read"),
    ("NodeStatus", "Permission::Read"),
    ("Session", "Permission::Read"),
    ("Shutdown", "Permission::Admin"),
    ("StartTime", "Permission::Read"),
    ("StateAccountKey", "Permission::Read"),
    ("StateCall", "Permission::Read"),
    ("StateCirculatingSupply", "Permission::Read"),
    ("StateDealProviderCollateralBounds", "Permission::Read"),
    ("StateFetchRoot", "Permission::Read"),
    ("StateGetActor", "Permission::Read"),
    ("StateGetBeaconEntry", "Permission::Read"),
    ("StateGetRandomnessFromBeacon", "Permission::Read"),
    ("StateGetRandomnessFromTickets", "Permission::Read"),
    ("StateGetReceipt", "Permission::Read"),
    ("StateListMessages", "Permission::Read"),
    ("StateListMiners", "Permission::Read"),
    ("StateLookupID", "Permission::Read"),
    ("StateMarketBalance", "Permission::Read"),
    ("StateMarketDeals", "Permission::Read"),
    ("StateMarketStorageDeal", "Permission::Read"),
    ("StateMinerActiveSectors", "Permission::Read"),
    ("StateMinerAvailableBalance", "Permission::Read"),
    ("StateMinerDeadlines", "Permission::Read"),
    ("StateMinerFaults", "Permission::Read"),
    ("StateMinerInfo", "Permission::Read"),
    ("StateMinerPartitions", "Permission::Read"),
    ("StateMinerPower", "Permission::Read"),
    ("StateMinerProvingDeadline", "Permission::Read"),
    ("StateMinerRecoveries", "Permission::Read"),
    ("StateMinerSectorCount", "Permission::Read"),
    ("StateMinerSectors", "Permission::Read"),
    ("StateNetworkName", "Permission::Read"),
    ("StateNetworkVersion", "Permission::Read"),
    ("StateReadState", "Permission::Read"),
    ("StateReplay", "Permission::Read"),
    ("StateSearchMsg", "Permission::Read"),
    ("StateSearchMsgLimited", "Permission::Read"),
    ("StateSectorGetInfo", "Permission::Read"),
    ("StateSectorPreCommitInfo", "Permission::Read"),
    ("StateVerifiedClientStatus", "Permission::Read"),
    ("StateVMCirculatingSupplyInternal", "Permission::Read"),
    ("StateWaitMsg", "Permission::Read"),
    ("SyncCheckBad", "Permission::Read"),
    ("SyncMarkBad", "Permission::Admin"),
    ("SyncState", "Permission::Read"),
    ("SyncSubmitBlock", "Permission::Write"),
    ("Version", "Permission::Read"),
    ("WalletBalance", "Permission::Read"),
    ("WalletDefaultAddress", "Permission::Read"),
    ("WalletDelete", "Permission::Write"),
    ("WalletExport", "Permission::Admin"),
    ("WalletHas", "Permission::Write"),
    ("WalletImport", "Permission::Admin"),
    ("WalletList", "Permission::Write"),
    ("WalletNew", "Permission::Write"),
    ("WalletSetDefault", "Permission::Write"),
    ("WalletSign", "Permission::Sign"),
    ("WalletValidateAddress", "Permission::Read"),
    ("WalletVerify", "Permission::Read"),
    ("Web3ClientVersion", "Permission::Read"),
];
