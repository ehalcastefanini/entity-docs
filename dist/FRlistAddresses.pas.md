<!-- tabs:start -->

#### **Documentation**

# Documentation for `FRlistAddresses` Code Unit

---

## 1. Overview:

### Objective and Problem Solved:
The `FRlistAddresses` code unit is designed to manage and display a grid of addresses and their associated contacts. It provides functionality for adding, modifying, and viewing address details. The main objective is to streamline the management of address data, which can be associated with various entities such as customers, consignees, carriers, agents, and warehouses.

### High-Level Functionality:
- Displays a grid of addresses.
- Allows users to add, modify, or view address details through a separate form (`MaddressAndContact`).
- Integrates with datasets for both addresses and contacts.
- Provides event-driven mechanisms to handle data retrieval and updates.

### Technologies Used:
- Delphi (Object Pascal) for application development.
- VCL (Visual Component Library) for UI components.
- SOAP for communication with external services.
- `TClientDataSet` for managing datasets.

### Form Type:
This is a **grid display**.  
- **Grid Columns and Types:**
  - Address details (e.g., Address ID, Address Name, etc.).
  - Contact details (linked via `CDScontacts` dataset).
- **Grid Actions and Effects:**
  - Double-clicking a cell opens the address editor form.
  - Buttons for adding and modifying addresses trigger respective actions.

---

## 2. Functionality Description:

### User/Software Actions:
- **Add Address:** Opens the address editor form to create a new address.
- **Modify Address:** Opens the address editor form to edit the selected address.
- **View Address Details:** Double-clicking a grid cell opens the address editor form in view mode.

### Main Components:
- **Grid (`cxGrid`):** Displays the list of addresses.
- **Buttons (`BTNadd`, `BTNmodify`):** Trigger actions for adding and modifying addresses.
- **Datasets (`CDStable`, `CDScontacts`):** Manage address and contact data.

### Pseudo-Code for Actions and Events:
- **Add Address Button Click:**
  ```
  if add button clicked then
    open address editor form in add mode
  ```
- **Modify Address Button Click:**
  ```
  if modify button clicked then
    open address editor form in edit mode
  ```
- **Grid Cell Double-Click:**
  ```
  if grid cell double-clicked then
    open address editor form in view mode
  ```

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization:**
   - The frame is initialized with datasets for addresses and contacts.
   - The `MasterKeyFields` property is set based on the parent form's configuration.
2. **User Interaction:**
   - Users interact with the grid or buttons to perform actions.
   - Events are triggered to open the address editor form or handle data updates.

### Functions and File Locations:
- **`CreateAndCallEditor` (FRlistAddresses.pas):** Opens the address editor form with the specified parameters.
- **`m_CheckPostalCodes` (FRlistAddresses.pas):** Validates postal codes for the addresses.
- **`m_ProcessEachRow` (FRlistAddresses.pas):** Processes each row in the grid.

### Required User Data:
- Address details (e.g., street, city, postal code).
- Contact details (if applicable).

---

## 4. Business Rules:

### Actions and Preconditions:
- **Add Address:** No preconditions; always enabled.
- **Modify Address:** Requires a row to be selected in the grid.
- **View Address Details:** Requires a row to be double-clicked in the grid.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- No explicit error messages are defined in the code.

### Default Field Values:
- Not explicitly defined in the code.

### Field Validation and Conditions:
- No explicit validations are defined in the code.

---

## 5. Main Functions:

1. **`CreateAndCallEditor`:** Opens the address editor form with the specified address number and save point.
2. **`m_CheckPostalCodes`:** Validates postal codes for the addresses in the dataset.
3. **`m_ProcessEachRow`:** Iterates through each row in the grid and processes it based on custom logic.

---

## 6. API Service Consumption:

No explicit API service calls are defined in the code.

---

## 7. Conditional Fields (Form Logic):

- The code does not define any conditional fields.

---

## 8. Dependencies:

### External Libraries:
- **VCL Components:** Used for UI elements like grids, panels, and buttons.
- **SOAP Components:** Used for communication with external services.

### Custom Components:
- **`TFRAMEBaseGridEditSOA`:** Base class for the frame, providing grid editing functionality.
- **`TFORMMaddressAndContact`:** Custom form for editing address and contact details.

---

## 9. Fields and Validations Listing:

- **Address Fields:** Not explicitly defined in the code.
- **Contact Fields:** Managed via the `CDScontacts` dataset.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [Initialize Frame] --> [Load Datasets]
   --> [User Interaction] --> [Trigger Event] --> [Open Editor Form]
```

### Sequence Diagram:
```plaintext
User --> Grid: Double-click cell
Grid --> Frame: Trigger event
Frame --> Editor Form: Open with parameters
```

### Code Snippets:
```pascal
procedure TFRAMElistAddresses.BTNmodifyClick(Sender: TObject);
begin
  CreateAndCallEditor(SelectedAddressID, SavePoint);
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

- **Frame Functionality:**
  ```plaintext
  // This frame manages two datasets:
  // 1. CDStable: Default dataset for addresses.
  // 2. CDScontacts: Reference to the contacts dataset.
  ```
- **Initialization Logic:**
  ```plaintext
  // The frame initializes the MasterKeyFields property based on the parent form.
  ```

---

## 12. Conclusion:

The `FRlistAddresses` code unit provides a robust framework for managing address data within a grid interface. Its integration with datasets and the address editor form ensures seamless data management. However, the lack of explicit error handling, field validations, and filters may limit its usability in more complex scenarios.

---

## 13. Short Summary:

The `FRlistAddresses` unit manages address data in a grid interface, allowing users to add, modify, and view details. It integrates with datasets and an editor form for seamless data handling, making it suitable for managing addresses linked to various entities.#### **FRlistAddresses.pas**

```
unit FRlistAddresses;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneFRGridEditSOA, cxStyles, cxCustomData, cxGraphics, cxFilter,
  cxData, cxDataStorage, cxEdit, DB, cxDBData, InvokeRegistry,
  cxEditRepositoryItems, sFrameAdapter, ImgList, ActnList, ExtCtrls, Rio,
  SOAPHTTPClient, DBClient, kneFRGridManager, StdCtrls, Buttons, sBitBtn,
  sPanel, cxGridLevel, cxClasses, cxControls, cxGridCustomView,
  cxGridCustomTableView, cxGridTableView, cxGridDBTableView, cxGrid, kneUtils,
  MaddressAndContact;

type
  TFRAMElistAddresses = class(TFRAMEBaseGridEditSOA)
    PNLmodify: TsPanel;
    BTNmodify: TsBitBtn;
    procedure ACTaddExecute(Sender: TObject);
    procedure BTNmodifyClick(Sender: TObject);
    procedure cxDBVtableCellDblClick(Sender: TcxCustomGridTableView;
      ACellViewInfo: TcxGridTableDataCellViewInfo; AButton: TMouseButton;
      AShift: TShiftState; var AHandled: Boolean);
  private
    FGetDataSet: TNotifyEvent;
    FformEditAddrs: TFORMMaddressAndContact;   // referencia para o form de edi��o de Address e Contracts
    { Private declarations }
    procedure SetGetDataSet(const Value: TNotifyEvent);
    procedure CreateAndCallEditor(pv_addrNum, pv_AddressSavePoint: Integer);
    procedure m_OnSetAccessMode(Sender: TObject; var pv_State: Boolean);
    procedure m_ProcessEachRow(pv_RowIndex: Integer;
      pv_RowInfo: TcxRowInfo);

  protected
    { protected declarations}
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
//    procedure LoadRecords(Sender: TObject);
//    function GetAddrNum: string;
//    function GetEntityType: string;
//    function GetEntityCode: string;

    procedure m_CheckPostalCodes;


  published
//    property entityCode: string read GetEntityCode;
//    property entityType: string read GetEntityType;
//    property addrNum: string read GetAddrNum;
//    function GetAddressMasterCDS: TClientDataSet;      // devolve uma referencia para o DS de Addresses
//    function GetAddressContactsCDS: TClientDataSet;    // devolve uma referencia para o DS de Contacts
    CDScontacts: TClientDataSet;  // referencia para o dataset de contacts da frame FRlistContacts
    property GetDataSet: TNotifyEvent read FGetDataSet write SetGetDataSet;  // Evento para obter a referencia do CDS de contacts
  end;

var
  FRAMElistAddresses: TFRAMElistAddresses;

implementation

uses
  kneTypes
  , Global {#24241};

{$R *.dfm}

{
// -----------------------------------------------------------------------------
//     Funcionamento da Frame e sua Estrutura
// -----------------------------------------------------------------------------
  Esta frame tem 2 dataset:
   1: CDStable -> Dataset default da frame (que tem os addresses)
   2: CDScontacts -> referencia para o DS da Frame(FRlistContacts)

  Esta frame chama o form (MaddressAndContact) sempre que seja para Inserir,
  alterar, ou visualizar um address e os seus detalhes.
  O form usa os datasets que existem nesta frame e o CDScontacts
  que � uma referencia para o da frame (FRlistContacts).
  Esta frame instancia as referencias no form para os CDS's.
  Para obter o ponteiro para o CDS da frame de contacts � utilizada um evento ()
  que dispara o pedido de preenchimento do pt. do dataset(CDScontacts) de Contacts
}

constructor TFRAMElistAddresses.Create(AOwner: TComponent);
var
  lv_DefaultAction: Boolean;
begin
  inherited;

  // SET DAS PROPRIEDADES DA FRAME
  // As frames do address s�o utilizadas em diversas entidades
  //    (customer, consignee, carrier, agent, warehouse)
  // e em cada uma delas a chave de liga��o do detail (address) com a entidade
  // master � diferente. Para ultrapassar esta frame ao ser criada
  // consulta esta propriedade do form master e inicializa o masterKeyField
  // com o valor obtido, sen�o existir define o valor padr�o
  if TkneTypeInfo.fg_HasProperty(Owner, 'AddressMasterKeyFields') then
    MasterKeyFields := TkneTypeInfo.fg_GetStringPropertyValue(Owner, 'AddressMasterKeyFields')
  else
```

#### **FRlistAddresses.dfm**

```
inherited FRAMElistAddresses: TFRAMElistAddresses
  ParentFont = True
  inherited cxDBG: TcxGrid
    inherited cxDBVtable: TcxGridDBTableView
      OnCellDblClick = cxDBVtableCellDblClick
      OptionsData.Deleting = False
      OptionsData.Editing = False
      OptionsData.Inserting = False
    end
  end
  inherited PNLfooter: TsPanel
    inherited PNLeditActions: TsPanel
      Left = 91
      Width = 318
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
```
<!-- tabs:end -->

