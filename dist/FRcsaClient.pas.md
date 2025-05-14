<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRcsaClient` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `FRcsaClient` code unit defines a Delphi frame (`TFRAMEcsaClient`) that provides a user interface for managing and transferring client data. It includes functionalities for selecting clients, transferring them, and managing related data. The frame is designed to work with a grid-based display of client information, allowing users to interact with the data efficiently.

### Technologies Used:
- **Delphi VCL Framework**: For creating the user interface and handling events.
- **SOAP Services**: For interacting with external services (`TBoAssistServiceUtils`).
- **Database Components**: For managing and displaying data (`TcxGridDBTableView`, `DBClient`).
- **Third-party Libraries**: Includes `cxStyles`, `cxGrid`, and `sPanel` for enhanced UI components.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
- `custCd` (Customer Code): String
- `abbrName` (Abbreviated Name): String
- `name` (Full Name): String
- `countryCd` (Country Code): String
- `cyDescrip` (Country Description): String
- `languageCd` (Language Code): String
- `language` (Language): String
- `legalNum` (Legal Number): String
- `custMaster` (Customer Master): String
- `master` (Master): String
- `custTypeCd` (Customer Type Code): String
- `description` (Description): String
- `groupCd` (Group Code): String
- `gName` (Group Name): String
- `marketCd` (Market Code): String
- `maDescrip` (Market Description): String
- `paymentCd` (Payment Code): String
- `plDescrip` (Payment Description): String
- `delTerms` (Delivery Terms): String
- `dtDescrip` (Delivery Terms Description): String
- `salesAssist` (Sales Assistant): String
- `salesAssistDesc` (Sales Assistant Description): String
- `salesmanCd` (Salesman Code): String
- `salesman` (Salesman): String
- `entityTp` (Entity Type): String

#### Grid Actions and Their Effects:
- **Select All**: Selects all rows in the grid.
- **Select None**: Deselects all rows in the grid.
- **Transfer**: Opens the transfer panel for selected clients.
- **Do Transfer**: Executes the transfer operation for selected clients.
- **Cancel Transfer**: Cancels the transfer operation.

---

## 2. Functionality Description:

### User Actions:
1. **Select All**: Selects all clients in the grid.
2. **Select None**: Deselects all clients in the grid.
3. **Transfer**: Opens the transfer panel for further actions.
4. **Do Transfer**: Transfers the selected clients.
5. **Cancel Transfer**: Cancels the transfer operation.

### Main Components:
- **Grid (`cxGrid`)**: Displays client data.
- **Buttons (`TsBitBtn`)**: For user actions like selecting, transferring, and canceling.
- **Panels (`TsPanel`)**: Organizes UI elements.
- **Memo (`TsMemo`)**: For user remarks during transfer.
- **Labels (`TsLabel`)**: For displaying static text.

### Pseudo-code for Actions and Events:
- `OnClick` event of `BTNselectAll`: `if button clicked then select all rows in the grid`.
- `OnClick` event of `BTNselectNone`: `if button clicked then deselect all rows in the grid`.
- `OnClick` event of `BTNtransfer`: `if button clicked then show transfer panel`.
- `OnClick` event of `BTNdoTransfer`: `if button clicked then execute client transfer`.
- `OnClick` event of `BTNcancelTransfer`: `if button clicked then cancel transfer operation`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is initialized with default settings in the `Create` constructor.
   - Grid settings are configured, including field visibility, read-only fields, and custom fields.

2. **User Interactions**:
   - Users interact with buttons to select/deselect clients or initiate transfer operations.
   - The grid displays client data, and users can view or modify selections.

3. **Functions**:
   - `m_PrepareFrame`: Prepares the frame for transfer operations.
   - `m_TransferClients`: Executes the transfer of selected clients.
   - `m_SetFindCSA`: Configures the CSA search functionality.

### Data Requirements:
- Users must select clients in the grid to perform transfer operations.
- Remarks can be optionally provided in the memo field.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Select All**: No preconditions; selects all rows.
- **Select None**: No preconditions; deselects all rows.
- **Transfer**: Requires at least one client to be selected.
- **Do Transfer**: Requires at least one client to be selected and the transfer panel to be open.
- **Cancel Transfer**: Requires the transfer panel to be open.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- Not explicitly defined in the code.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- Not explicitly defined in the code.

---

## 5. Main Functions:

1. **`m_PrepareFrame`**:
   - Prepares the frame for transfer operations.
   - Configures visibility and settings.

2. **`m_TransferClients`**:
   - Executes the transfer of selected clients.

3. **`m_SetFindCSA`**:
   - Configures the CSA search functionality.

---

## 6. API Service Consumption:

- **Service Name**: `BoAssistServiceUtils`
- **Purpose**: Provides utility functions for managing client data.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- No conditional fields are explicitly defined in the code.

---

## 8. Dependencies:

### External Libraries:
- `cxGrid`, `cxStyles`: For grid and UI styling.
- `SOAPHTTPClient`: For SOAP service interactions.

### Custom Components:
- `TFRAMEFindEditSOA`: A custom frame for CSA search functionality.

---

## 9. Fields and Validations Listing:

- **Grid Fields**: Listed in Section 1.
- **Validations**: Not explicitly defined in the code.

---

## 10. Examples and Diagrams:

### Flowchart:
Not applicable.

### Sequence Diagram:
Not applicable.

### Code Snippets:
```pascal
procedure TFRAMEcsaClient.BTNselectAllClick(Sender: TObject);
begin
  // Select all rows in the grid
end;

procedure TFRAMEcsaClient.BTNdoTransferClick(Sender: TObject);
begin
  // Execute client transfer
end;
```

### Screenshots:
Not applicable.

---

## 11. Important Comments in the Code:

- **Grid Settings**: Configures field visibility, read-only fields, and custom fields.
- **Initialization**: Sets up the frame with default properties and settings.

---

## 12. Conclusion:

The `FRcsaClient` code unit provides a robust framework for managing and transferring client data. Its grid-based interface and SOAP service integration make it suitable for enterprise applications. However, the lack of explicit error handling and field validations may require additional implementation.

---

## 13. Short Summary:

The `FRcsaClient` unit defines a grid-based interface for managing and transferring client data, with functionalities like selection, transfer, and SOAP service integration. It is designed for enterprise use but lacks explicit error handling and field validations.#### **FRcsaClient.pas**

```
unit FRcsaClient;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid,
  kneFRFindEditSOA, sLabel, Grids, DBGrids, sMemo;

type
  TFRAMEcsaClient = class(TFRAMEBaseGridEditSOA)
    PNLselectionArea: TsPanel;
    BTNselectAll: TsBitBtn;
    BTNselectNone: TsBitBtn;
    cxEDTRchkSel: TcxEditRepositoryCheckBoxItem;
    PNLtransfer: TsPanel;
    BTNtransfer: TsBitBtn;
    PNLeditor: TsPanel;
    BTNdoTransfer: TsBitBtn;
    BTNcancelTransfer: TsBitBtn;
    LBL2: TsLabel;
    FRAMEFindCSA: TFRAMEFindEditSOA;
    MMOremarks: TsMemo;
    LBL1: TsLabel;
    PNL1: TsPanel;
    procedure BTNselectAllClick(Sender: TObject);
    procedure BTNselectNoneClick(Sender: TObject);
    procedure BTNdoTransferClick(Sender: TObject);
    procedure BTNcancelTransferClick(Sender: TObject);
    procedure BTNtransferClick(Sender: TObject);
  private
    procedure m_PrepareFrame(pv_Transfer: Boolean);
    procedure m_TransferClients;
    procedure m_SetFindCSA;
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;
  end;

var
  FRAMEcsaClient: TFRAMEcsaClient;

implementation

{$R *.dfm}

uses
  kneUtils, kneTypes
  , BoAssistServiceUtils, kneFREditSOA;

const
  mc_GRID_FIELDS = 'custCd;abbrName;name;countryCd;cyDescrip;languageCd'
    + ';language;legalNum;custMaster;master;custTypeCd;description'
    + ';groupCd;gName;marketCd;maDescrip;paymentCd;plDescrip'
    + ';delTerms;dtDescrip;salesAssist;salesAssistDesc;salesmanCd;salesman'
    + ';entityTp';
                         
{ TFRAMEcsaClient }

constructor TFRAMEcsaClient.Create(AOwner: TComponent);
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'boAssist=boAssistCd';
  DataPacketName := 'CsaClient';
  PropertyName := 'csaClients';
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := True;
  AvailableActions := '';

  // setup custom fields
  ProviderService := TBoAssistServiceUtils.Create(self);

  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin

    DefineOrderFields('selected;' + mc_GRID_FIELDS);

    DefineReadonlyFields(mc_GRID_FIELDS + ';abbrName');

    DefineHiddenFields('HIDE_ALL_FIELDS');

    KeyFields:= 'boAssistCd; custCd';

    AddCustomField('selected','cxEDTRchkSel');
  end; //with

  ActiveDatasetEvents := []; // para que n�o despolete os eventos da base associados ao DS
```

#### **FRcsaClient.dfm**

```
inherited FRAMEcsaClient: TFRAMEcsaClient
  ParentFont = True
  inherited cxDBG: TcxGrid
    Top = 24
    Height = 95
    inherited cxDBVtable: TcxGridDBTableView
      DataController.DataModeController.GridMode = False
      DataController.Filter.AutoDataSetFilter = True
    end
  end
  inherited PNLfooter: TsPanel
    inherited PNLeditActions: TsPanel
      Width = 415
      object PNLtransfer: TsPanel
        Left = 321
        Top = 1
        Width = 91
        Height = 30
        Align = alLeft
        TabOrder = 4
        SkinData.SkinSection = 'ALPHACOMBOBOX'
        object BTNtransfer: TsBitBtn
          Left = 3
          Top = 3
          Width = 81
          Height = 24
          Caption = 'Transfer'
          TabOrder = 0
          OnClick = BTNtransferClick
          SkinData.SkinSection = 'BUTTON'
          ImageIndex = 6
          Images = IMLeditActions
        end
      end
    end
  end
  object PNLselectionArea: TsPanel [2]
    Left = 0
    Top = 0
    Width = 435
    Height = 24
    Align = alTop
    TabOrder = 2
    SkinData.SkinSection = 'PANEL'
    object BTNselectAll: TsBitBtn
      Left = 6
      Top = 2
      Width = 20
      Height = 20
      Hint = 'Select All'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
      OnClick = BTNselectAllClick
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
        C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6
        A4C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEFCFBFEFCFBFEFCFBFEFCFBFE
        FCFBFEFCFBFEFCFBFEFCFBFEFCFBFEFCFBC2A6A4FF00FFFF00FFFF00FFFF00FF
        C2A6A4FEFCFB993300993300993300993300993300993300993300993300FEFC
        FBC2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEFBF7993300FEFEFEFEFEFEFE
        FEFE8EA4FDB8C6FDFEFEFE993300FEFBF7C2A6A4FF00FFFF00FFFF00FFFF00FF
        C2A6A4FEF9F4993300FEFEFEFAFBFE7E98FC0335FB597AFCFEFEFE993300FEF9
        F4C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEF7F0993300D6DEFE4368FC03
        35FB4066FC0436FBD9E0FE993300FEF7F0C2A6A4FF00FFFF00FFFF00FFFF00FF
        C2A6A4FEF5EC9933005274FC1442FBBCC9FDEFF2FE1A47FB4F72FC973304FEF5
        ECC2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEF3E9993300E4EAFED9E0FEFE
        FEFEFEFEFE98ACFD0335FB643459FEF3E9C2A6A4FF00FFFF00FFFF00FFFF00FF
        C2A6A4FFF1E5993300FEFEFEFEFEFEFEFEFEFEFEFEFEFEFE5677FC0335FBFFF1
        E5C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FFF0E299330099330099330099
        33009933009933008F33112235C80335FBC2A6A4FF00FFFF00FFFF00FFFF00FF
        C2A6A4FFEEDEFFEEDEFFEEDEFFEEDEFFEEDEFFEEDEC5B5A9C3B4A8C2B3A70335
        FB0335FB0335FBFF00FFFF00FFFF00FFC2A6A4FFECDAFFECDAFFECDAFFECDAFF
        ECDAFFECDAB0A296B0A296B0A296B0A296C2A6A40335FBFF00FFFF00FFFF00FF
        C2A6A4FFEAD7FFEAD7FFEAD7FFEAD7FFEAD7C9B9ACFBF8F4FBF8F4E6DAD9C2A6
        A4FF00FFFF00FFFF00FFFF00FFFF00FFC2A6A4FFE8D3FFE8D3FFE8D3FFE8D3FF
        E8D3C9B9ACFBF8F4DFCEC7C2A6A4FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF
        C2A6A4FFE6D0FFE6D0FFE6D0FFE6D0FFE6D0C9B9ACDFCEC7C2A6A4FF00FFFF00
        FFFF00FFFF00FFFF00FFFF00FFFF00FFC2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2
        A6A4C2A6A4C2A6A4FF00FFFF00FFFF00FFFF00FFFF00FFFF00FF}
      Alignment = taLeftJustify
      SkinData.SkinSection = 'SPEEDBUTTON'
      ImageIndex = 7
    end
    object BTNselectNone: TsBitBtn
      Left = 29
      Top = 2
      Width = 20
      Height = 20
      Hint = 'Select None'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      OnClick = BTNselectNoneClick
      Glyph.Data = {
        36030000424D3603000000000000360000002800000010000000100000000100
        18000000000000030000120B0000120B00000000000000000000FF00FFFF00FF
        C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6A4C2A6
        A4C2A6A4FF00FFFF00FFFF00FFFF00FFC2A6A4FEFCFBFEFCFBFEFCFBFEFCFBFE
```
<!-- tabs:end -->

