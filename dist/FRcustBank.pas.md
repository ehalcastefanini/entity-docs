<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRcustBank` Code Unit

---

## 1. Overview:

### Objective and Problem Solved:
The `FRcustBank` code unit defines a frame (`TFRAMEcustBank`) that manages a grid-based interface for displaying and editing customer bank information. It provides functionality for managing customer-bank relationships, including searching for banks, editing bank details, and configuring grid settings. The main objective is to streamline the management of customer bank data in a structured and user-friendly interface.

### Technologies Used:
- **Delphi VCL Framework**: For creating the user interface and handling events.
- **SOAP Services**: For interacting with external services (e.g., `BankServiceUtils`).
- **Database Components**: For managing and displaying data from a database (`DBClient`, `cxDBData`).
- **Custom Grid Components**: `cxGrid`, `cxGridDBTableView` for displaying and editing data in a tabular format.

### Form Type:
This is a **grid display** form.

#### Grid Columns and Their Types:
1. **mill**: Hidden field.
2. **bankCode**: Editable field with a custom editor (`cxEDTfind`).
3. **bankName**: Display field.
4. **stat**: Editable field with a custom editor (`cxEDTstat`).
5. **lastUpd**: Display field.
6. **updBy**: Display field.

#### Grid Actions and Their Effects:
- **Edit Value Changed**: Triggers when a value in the grid is edited.
- **Find Bank**: Opens a dialog to search for a bank.
- **Set Key Editing**: Configures whether the key fields are editable.

---

## 2. Functionality Description:

### User/Software Actions:
1. **Edit Grid Values**: Users can edit specific fields in the grid.
2. **Search for Banks**: Users can search for banks using a custom dialog.
3. **Configure Grid Settings**: The grid can be customized (e.g., hiding fields, setting field order).

### Main Components:
- **Grid (`cxGrid`)**: Displays customer bank data.
- **Custom Editors**: Provides specialized input fields for certain columns.
- **Find Dialog**: Allows users to search for banks.

### Pseudo-Code for Actions and Events:
- `OnEditValueChanged` event: `if grid value changed then execute value change logic`.
- `OnButtonClick` event of `cxEDTfind`: `if button clicked then open find dialog`.
- `SetKeyEditing` method: `if editing enabled then allow key field editing`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The frame is created (`Create` constructor).
   - Grid settings are configured (e.g., hidden fields, field order, key fields).
   - Custom editors are assigned to specific fields.

2. **User Interactions**:
   - Users can edit grid values, triggering the `OnEditValueChanged` event.
   - Users can click the search button in the `bankCode` field, opening the find dialog.

### Functions and File Locations:
- **`Create` Constructor** (in `FRcustBank`):
  - Initializes the frame and configures grid settings.
- **`m_SetFindEditBank` Method** (in `FRcustBank`):
  - Opens the find dialog for searching banks.
- **`SetKeyEditing` Method** (in `FRcustBank`):
  - Configures whether key fields are editable.

### Required Data:
- **Customer Code**: Used to filter and display relevant bank data.
- **Bank Code**: Used for searching and editing bank details.

---

## 4. Business Rules:

### Actions and Preconditions:
- **Edit Grid Values**: Users can edit fields unless they are marked as read-only.
- **Search for Banks**: Requires the `bankCode` field to be selected.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- No explicit error messages are defined in the code.

### Default Field Values:
- No default values are explicitly defined in the code.

### Field Validation and Conditions:
- **bankCode**: Uses a custom editor (`cxEDTfind`) for searching.
- **stat**: Uses a custom editor (`cxEDTstat`) for status selection.

---

## 5. Main Functions:

1. **`Create` Constructor**:
   - Initializes the frame and configures grid settings.
2. **`m_SetFindEditBank`**:
   - Opens a dialog for searching banks.
3. **`SetKeyEditing`**:
   - Configures whether key fields are editable.

---

## 6. API Service Consumption:

- **Service Name**: BankServiceUtils.
- **Endpoint**: Not explicitly defined in the code.
- **Data Sent**: Not explicitly defined in the code.
- **Data Received**: Not explicitly defined in the code.
- **Purpose**: Likely used for retrieving bank data.
- **Error Handling**: Not explicitly defined in the code.

---

## 7. Conditional Fields (Form Logic):

- **bankCode**: Uses a custom editor (`cxEDTfind`) for searching.
- **stat**: Uses a custom editor (`cxEDTstat`) for status selection.

---

## 8. Dependencies:

### External Libraries:
- **SOAPHTTPClient**: For SOAP service communication.
- **cxGrid**: For grid-based data display.
- **DBClient**: For database interaction.

### Custom Components:
- **`cxEDTfind`**: Custom editor for searching banks.
- **`cxEDTstat`**: Custom editor for status selection.

---

## 9. Fields and Validations Listing:

1. **mill**: Hidden field.
2. **bankCode**: Editable, uses a custom editor (`cxEDTfind`).
3. **bankName**: Display field.
4. **stat**: Editable, uses a custom editor (`cxEDTstat`).
5. **lastUpd**: Display field.
6. **updBy**: Display field.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Frame] --> [Configure Grid Settings] --> [User Interaction]
    --> [Edit Grid Values] --> [Trigger Events] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Grid: Edit Value
Grid --> Event Handler: Trigger OnEditValueChanged
Event Handler --> Grid: Update Value
```

### Code Snippets:
```delphi
procedure TFRAMEcustBank.m_SetFindEditBank(Sender: TObject; AButtonIndex: Integer);
begin
  lv_Find := TkneDialogFactory.GetFindDialog(Application);
  lv_Find.Options.DataSelection.FieldNameForCode := 'bankCode';
  lv_Find.ShowModal;
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

- **Grid Settings**:
  - Hidden fields: `DefineHiddenFields('HIDE_ALL_FIELDS')`.
  - Field order: `DefineOrderFields('mill; bankCode; bankName; stat; lastUpd; updBy')`.
  - Key fields: `KeyFields := 'customer;mill;bankCode'`.

- **Custom Editors**:
  - `AddCustomField('bankCode', 'cxEDTfind')`.
  - `AddCustomField('stat', 'cxEDTstat')`.

---

## 12. Conclusion:

The `FRcustBank` code unit provides a robust framework for managing customer-bank relationships through a grid-based interface. It supports custom field editors, search functionality, and configurable grid settings. However, the code lacks explicit error handling, default values, and detailed API integration.

---

## 13. Short Summary:

The `FRcustBank` unit defines a grid-based interface for managing customer-bank relationships, supporting custom field editors, search functionality, and configurable settings. It integrates with SOAP services for data retrieval and updates.#### **FRcustBank.pas**

```
unit FRcustBank;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid;

type
  TFRAMEcustBank = class(TFRAMEBaseGridEditSOA)
    procedure cxDBVtableEditValueChanged(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
  private
    { Private declarations }
    procedure m_FindByCodeBank(Sender: TcxCustomGridTableView;
      AItem: TcxCustomGridTableItem);
    procedure m_SetFindEditBank(Sender: TObject; AButtonIndex: Integer);
    procedure SetKeyEditing(const EditKey: Boolean);  override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent);  override;

    procedure SetForDOCADDR;
  end;

var
  FRAMEcustBank: TFRAMEcustBank;

implementation

{$R *.dfm}

uses
  kneUtils, kneTypes, kneConfigObjects, kneFindDialog, kneDialogFactory,
  kneFGFindUtils, Global,
  //---
  BankServiceUtils;

{ TFRAMEcustBank }

constructor TFRAMEcustBank.Create(AOwner: TComponent);
begin
  inherited;
  // SET DAS PROPRIEDADES DA FRAME
  MasterKeyFields := 'customerCode=customer';
  DataPacketName := 'CustomerBank';
  PropertyName := 'banks';
  FrameType := frtDetail;

  // configurar visibilidade de painel de ac��es e ac��es dispon�veis
  // pode ser usado em qualquer sitio do c�digo e a frame � imediatamente
  // redesenhada
  ShowActionPanel := False;
  AvailableActions := '';
  PNLfooter.Visible := False;
    
  //Definir aqui os settings da dxGrid
  with GridSettings do
  begin
    // Campos Read-Only ........................................................
    // Campos Hidden ...........................................................
    DefineHiddenFields('HIDE_ALL_FIELDS');
    // Ordem Campos ............................................................
    DefineOrderFields('mill; bankCode; bankName; stat; lastUpd; updBy');
    // Key Fields ..............................................................
    KeyFields:= 'customer;mill;bankCode';
    // Custom Editors ..........................................................
    AddCustomField('bankCode','cxEDTfind');
    AddCustomField('stat','cxEDTstat');
  end; //with

  cxEDTfind.Properties.OnButtonClick := m_SetFindEditBank;
end;

procedure TFRAMEcustBank.m_SetFindEditBank(Sender: TObject;
  AButtonIndex: Integer);
var
  lv_Find: TFORMkneFindDialog;
begin

  try    // inicializa��o de Find Dialog
    lv_Find := nil;
    lv_Find := TkneDialogFactory.GetFindDialog(Application);

    with lv_Find.Options.DataSelection do
    begin
      // campos para selec��o do Find DataSet
      FieldNameForCode:= 'bankCode';
      DefineFieldsForDesc('name; stat;');

      TargetDataSet:= CDStable;
      TargetFieldNameForCode:= 'bankCode';
      DefineTargetFieldsForDesc('bankName; stat;');

      UseTargetDataSet:= True;
```

#### **FRcustBank.dfm**

```
inherited FRAMEcustBank: TFRAMEcustBank
  ParentFont = True
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnEditValueChanged = cxDBVtableEditValueChanged
    end
  end
  inherited PNLfooter: TsPanel
    inherited PNLeditActions: TsPanel
      inherited PNLaddAction: TsPanel
        inherited BTNadd: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331310063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            63003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            310063319C003131CE00315AE700315AE700315AE700315AE700315AE700315A
            E7003131CE003131630039181000FF00FF00FF00FF00FF00FF00FF00FF006331
            9C00315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00639CFF00315A
            E700315AE7003131CE003131630063313100FF00FF00FF00FF009C316300315A
            E700315AE700315AE700315AE7009C9CFF00FFFFFF00FFFFFF009C9CFF00315A
            E700315AE700315AE7003131CE0031313100FF00FF00FF00FF0063639C00315A
            E700315AE700315AE700315AE700A5B5F700FFFFFF00FFFFFF00A5B5F700315A
            E700315AE700315AE700315AE70031319C0063313100FF00FF00315AE700315A
            E700639CFF006363FF00639CFF00A5B5F700FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00639CFF00315AE7003131CE0063310000FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE700315AE70063313100FF00FF00315AE700315A
            E700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
            FF00FFFFFF00FFFFFF00315AE7003131CE007B392100FF00FF00315AE7003163
            FF00A5B5F700A5B5F700A5B5F700CEEFF700FFFFFF00FFFFFF00CEEFF700A5B5
            F700A5B5F700A5B5F700315AE700315AE7007B392100FF00FF006363CE00315A
            E7006363FF006363FF00639CCE00A5B5F700FFFFFF00FFFFFF00A5B5F7003163
            FF003163CE00315AE700315AE70031319C009C5A3900FF00FF00CE636300315A
            E700639CFF00639CFF00639CFF00B5D6E700FFFFFF00FFFFFF00A5B5F7003163
            FF003163FF003163FF00315AE70063316300FF00FF00FF00FF00FF00FF006363
            9C00315AE700639CFF009C9CFF00CECEFF00FFFFFF00FFFFFF00A5B5F700639C
            FF006363FF00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00
            FF0063639C00315AE700639CFF00A5B5F700B5D6E700A5B5F700639CFF006363
            CE00315AE70063319C00CE633100FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00CE6363006363CE00315AE7003163FF006363FF00315AE7006363
            CE009C636300CE633100FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLapplyAction: TsPanel
        inherited BTNapply: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF006331310063313100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF009C639C00A5B5F70031319C003131630031003100FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C316300F7F7F70063639C0000319C003131CE003131630063313100FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100639CCE006363CE0031319C00315AE700315AE70031319C0039181000FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF003131
            9C00315AE70031319C003163CE00315AE700315AE7003163CE00313163006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF0063316300315A
            E70031319C0031639C00315AE700315AE700315AE7003163FF0031319C003131
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00315AE7003131
            CE0031319C00639CFF00639CFF00639CFF00639CFF009C9CFF003163CE003131
            630063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00639CCE006363
            CE009C9CCE00639CFF009C9CFF00639CFF00639CCE00A5B5F700A5B5F7003163
            CE003131310063313100FF00FF00FF00FF00FF00FF00FF00FF009C639C00CECE
            CE00A5B5F700CECEFF00A5B5F700A5B5F7006363CE009C9CCE00CECEFF00639C
            FF0031319C0031313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF009C9C
            9C009C9CCE009C9CCE00B5D6E70063639C00CE313100A5B5F7009C9CCE00CEEF
            F700639CFF003131630039181000FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF009C639C009C639C009C316300FF00FF00FF00FF00CE636300CECECE009C9C
            CE00CECEFF003163CE003131630063313100FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE636300A5B5
            F7009C9CCE00CECEFF0031319C00313131007B392100FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            6300639CCE009C9CFF00B5D6E70031319C0094422900FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE63630063639C006363CE009C9CCE00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        end
      end
      inherited PNLcancelAction: TsPanel
        inherited BTNcancel: TsBitBtn
          Glyph.Data = {
            36040000424D3604000000000000360000002800000010000000100000000100
            2000000000000004000000000000000000000000000000000000FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00FF00FF00FF00FF009C5A39007B3921006331000063313100633100006331
            3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
            FF00CE6331006331630031319C003131CE003131CE003131CE003131CE003131
            9C003131310063313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
            3100633163003131CE00315AE700315AE700315AE700315AE700315AE700315A
            E7003131CE003131630063313100FF00FF00FF00FF00FF00FF00CE6331006331
```
<!-- tabs:end -->

