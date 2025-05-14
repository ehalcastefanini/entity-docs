<!-- tabs:start -->

#### **Documentation**

# Documentation for `EcustomerAddressDoc` Code Unit

## 1. Overview:

### Objective and Problem Solved:
The `EcustomerAddressDoc` code unit is designed to manage and display customer addresses and related documentation in a structured and user-friendly interface. It provides a form-based interface for users to view, edit, and manage customer address details and associated documentation. The main problem it solves is the need for a unified interface to handle address-related data for various entities (e.g., customers, consignees, carriers, agents, warehouses) while maintaining flexibility in linking address data to different master entities.

### Technologies Used:
- **Delphi VCL Framework**: Used for creating the graphical user interface and managing form components.
- **Third-party Components**: Includes components like `TsPageControl`, `TsSplitter`, `TFRAMElistAddresses`, and `TFRAMElistContacts` for enhanced UI functionality.
- **Custom Components**: Custom frames and panels for specific functionalities like address and contact management.

### Form Type:
This code represents a **form** with the following elements:
- **Form Elements**:
  - `PGCdetails` (Page Control): Contains tabs for "Address" and "Documentation."
  - `FRAMElistAddresses1` (Frame): Displays a list of addresses.
  - `FRAMElistContacts1` (Frame): Displays a list of contacts.
  - `FRAMEdocumentsInformation1` (Frame): Displays document-related information.
  - `SPL1` and `SPL2` (Splitters): Used for resizing sections of the form.
- **Form Actions**:
  - **Modify Button**: Allows editing of selected address or contact details.
  - **Navigation Tabs**: Switch between "Address" and "Documentation" views.

---

## 2. Functionality Description:

### User/Software Actions:
- View and manage customer addresses and associated documentation.
- Edit address and contact details.
- Navigate between "Address" and "Documentation" tabs.

### Main Components:
- **`PGCdetails`**: A page control with tabs for organizing address and documentation data.
- **`FRAMElistAddresses1`**: A frame for displaying and managing a list of addresses.
- **`FRAMElistContacts1`**: A frame for displaying and managing a list of contacts.
- **`FRAMEdocumentsInformation1`**: A frame for displaying document-related information.

### Pseudo-code for Actions and Events:
- **On Form Create**: `if form created then initialize components and set default values`.
- **On Tab Change**: `if tab changed then load corresponding data`.
- **On Modify Button Click**: `if modify button clicked then enable editing mode for selected item`.

---

## 3. Operational Logic:

### Execution Flow:
1. **Initialization**:
   - The form is created (`FormCreate`), and components are initialized.
   - Default tab is set to "Address."
2. **User Interaction**:
   - Users can navigate between tabs to view addresses or documentation.
   - Users can click the "Modify" button to edit selected address or contact details.
3. **Data Handling**:
   - Address and contact data are loaded dynamically based on the selected entity.

### Functions:
- **`FormCreate`**: Initializes the form and its components.
- **`m_CreateFormEdit`**: Creates an instance of the form for editing.
- **`m_FormEdit`**: Sets the form to editing mode with the specified access mode and key values.
- **`m_getData`**: Loads data into the form components.

### Required User Data:
- Address details (e.g., street, city, postal code).
- Contact details (e.g., name, phone, email).

---

## 4. Business Rules:

### Actions and Preconditions:
- **Modify Button**: Enabled only when an address or contact is selected.
- **Tab Navigation**: No preconditions; users can switch tabs freely.

### Available Filters:
- No explicit filters are defined in the code.

### Error Messages:
- Not explicitly defined in the code.

### Default Field Values:
- Default tab: "Address."
- Default `FAddressMasterKeyFields`: `'customer=entityCode;entityType'`.

### Field Validation and Conditions:
- Not explicitly defined in the code.

---

## 5. Main Functions:

- **`FormCreate`**: Initializes the form and sets default values.
- **`m_CreateFormEdit`**: Creates and returns an instance of the form for editing.
- **`m_FormEdit`**: Configures the form for editing mode.
- **`m_getData`**: Loads data into the form components.

---

## 6. API Service Consumption:

No external API calls are defined in the provided code.

---

## 7. Conditional Fields (Form Logic):

- The visibility of certain fields or frames (e.g., `FRAMElistAddresses1`, `FRAMElistContacts1`) depends on the selected tab in `PGCdetails`.

---

## 8. Dependencies:

### External Libraries:
- **Delphi VCL Components**: For UI elements and form management.
- **Third-party Components**: Includes `TsPageControl`, `TsSplitter`, and others for enhanced UI functionality.

### Custom Components:
- **`TFRAMElistAddresses`**: Custom frame for managing address lists.
- **`TFRAMElistContacts`**: Custom frame for managing contact lists.
- **`TFRAMEdocumentsInformation`**: Custom frame for managing document information.

---

## 9. Fields and Validations Listing:

### Fields:
- **AddressMasterKeyFields** (type: string, default: `'customer=entityCode;entityType'`).

### Mapping:
- Not explicitly defined in the code.

---

## 10. Examples and Diagrams:

### Flowchart:
```plaintext
[Start] --> [FormCreate] --> [Initialize Components] --> [Set Default Tab]
   --> [User Interaction] --> [Load Data Based on Tab] --> [End]
```

### Sequence Diagram:
```plaintext
User --> Form: Open Form
User --> Form: Navigate Tabs
User --> Form: Click Modify Button
Form --> Data: Load Data
```

### Code Snippets:
```delphi
procedure TFORMEcustomerAddressDoc.FormCreate(Sender: TObject);
begin
  inherited;
  PGCdetails.ActivePageIndex := 0; // Set default tab to "Address"
end;
```

### Screenshots:
Not applicable (no DFM file provided).

---

## 11. Important Comments in the Code:

- **`FAddressMasterKeyFields`**: Used to define the link between addresses and master entities.
- **`m_CreateFormEdit`**: Creates an instance of the form for editing.
- **`m_getData`**: Loads data into the form components.

---

## 12. Conclusion:

The `EcustomerAddressDoc` code unit provides a robust framework for managing customer addresses and related documentation. Its modular design allows for easy integration with various entities. However, the lack of explicit error handling and field validation may require additional implementation for production use.

---

## 13. Short Summary:

The `EcustomerAddressDoc` unit is a Delphi-based form for managing customer addresses and documentation. It features tabbed navigation, modular frames for address and contact management, and flexible linking to master entities.#### **EcustomerAddressDoc.pas**

```
unit EcustomerAddressDoc;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBEdit, ActnList, ImgList, knePrivileges, StdCtrls, Buttons,
  sBitBtn, sSpeedButton, ToolWin, ComCtrls, acCoolBar, ExtCtrls, sPanel,
  kneEnterAsTab, sPageControl, kneFREditSOA, kneFRCtrlEditSOA,
  FRcustomerAddressDoc, sSplitter, FRlistContacts, FRlistAddresses,
  kneFRGridEditSOA, FRdocumentsInformation, cxControls, cxSplitter;

type
  TFORMEcustomerAddressDoc = class(TFORMkneBaseEdit)
    PGCdetails: TsPageControl;
    TSHaddress: TsTabSheet;
    TSHdocumentation: TsTabSheet;
    PNLmaster: TsPanel;
    FRAMEdocumentsInformation1: TFRAMEdocumentsInformation;
    FRAMElistAddresses1: TFRAMElistAddresses;
    SPL2: TsSplitter;
    FRAMElistContacts1: TFRAMElistContacts;
    FRAMEcustomerAddressDoc1: TFRAMEcustomerAddressDoc;
    SPL1: TsSplitter;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
    FAddressMasterKeyFields: String;   // � necess�ria para definir a liga��o com os address ( uma vez que estes s�o utilizados por varias entidades)
    function GetAddrMasterKeyFields: String;
    procedure m_SetContactsDataSet(sender: TObject);

  protected
    procedure m_getData; override;
    function m_Validate: Boolean; override;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent);

    class function m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit; override;
    procedure m_FormEdit(const pv_AccessMode: string; const pv_KeyValues: string = '');  override;

  published
    property AddressMasterKeyFields: string read GetAddrMasterKeyFields; // � necess�ria para definir a liga��o com os address ( uma vez que estes s�o utilizados por varias entidades)
  end;

var
  FORMEcustomerAddressDoc: TFORMEcustomerAddressDoc;

implementation

uses
  kneUtils, Global;
  
{$R *.dfm}

{ TFORMEcustomerAddressDoc }


constructor TFORMEcustomerAddressDoc.Create(AOwner: TComponent);
begin
  // As frames do address s�o utilizadas em diversas entidades
  //    (customer, consignee, carrier, agent, warehouse)
  // e em cada uma delas a chave de liga��o do detail (address) com a entidade
  // master � diferente. Para ultrapassar isso a frame Address ao ser criada
  // consulta esta propriedade do form master e inicializa o masterKeyField dela
  // com este valor
  FAddressMasterKeyFields := 'customer=entityCode;entityType';
  
  inherited;

  PGCdetails.ActivePageIndex := 0;
end;      

procedure TFORMEcustomerAddressDoc.FormCreate(Sender: TObject);
begin
  inherited;
end;


// ################ FormCreate  ######################################
class function TFORMEcustomerAddressDoc.m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit;
begin
  // Substituir pelo nome do form
  Result := TFORMEcustomerAddressDoc.Create(Application);
end;

procedure TFORMEcustomerAddressDoc.m_FormEdit(const pv_AccessMode: string;   //JAR #5298  05-02-2010
      const pv_KeyValues: string = '');
begin
  StringAccessMode := 'MODIFY';
  inherited;
end;

procedure TFORMEcustomerAddressDoc.m_getData;
var
  lv_MasterFrame: TFRAMEBaseEditSOA;
begin
  Screen.Cursor := crHourGlass;
```

#### **EcustomerAddressDoc.dfm**

```
inherited FORMEcustomerAddressDoc: TFORMEcustomerAddressDoc
  Left = 300
  Top = 100
  Width = 878
  Height = 620
  Caption = 'Customer Address and Documentation'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object SPL1: TsSplitter [1]
    Left = 0
    Top = 191
    Width = 870
    Height = 4
    Cursor = crVSplit
    Align = alTop
    Color = clBtnFace
    ParentColor = False
    SkinData.SkinSection = 'FORM'
  end
  inherited PNLtoolbar: TsPanel
    Width = 870
    inherited CLBactions: TsCoolBar
      Width = 870
      Bands = <
        item
          Control = PNbotoes
          ImageIndex = -1
          MinHeight = 41
          Width = 866
        end>
      inherited PNbotoes: TsPanel
        Width = 853
        inherited PNnew: TsPanel
          Visible = False
        end
      end
    end
  end
  object PGCdetails: TsPageControl [3]
    Left = 0
    Top = 195
    Width = 870
    Height = 398
    ActivePage = TSHaddress
    Align = alClient
    TabOrder = 1
    SkinData.SkinSection = 'PANEL'
    object TSHaddress: TsTabSheet
      Caption = '&Address'
      SkinData.CustomColor = False
      SkinData.CustomFont = False
      object SPL2: TsSplitter
        Left = 0
        Top = 100
        Width = 862
        Height = 4
        Cursor = crVSplit
        Align = alTop
        Color = clBtnFace
        ParentColor = False
        SkinData.SkinSection = 'FORM'
      end
      inline FRAMElistAddresses1: TFRAMElistAddresses
        Left = 0
        Top = 0
        Width = 862
        Height = 100
        Align = alTop
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentBackground = False
        ParentFont = False
        TabOrder = 0
        inherited cxDBG: TcxGrid
          Width = 862
          Height = 66
        end
        inherited PNLfooter: TsPanel
          Top = 66
          Width = 862
          inherited PNLmodify: TsPanel
            inherited BTNmodify: TsBitBtn
              Left = 1
            end
          end
        end
      end
      inline FRAMElistContacts1: TFRAMElistContacts
        Left = 0
        Top = 104
        Width = 862
        Height = 266
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
```
<!-- tabs:end -->

