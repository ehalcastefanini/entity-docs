<!-- tabs:start -->

#### **Documentation**

## 1. Visão Geral:

* **Objetivo Principal:**  
  O código apresentado implementa um formulário de manutenção de clientes chamado `TFORMMcustomer`. Ele é utilizado para gerenciar informações relacionadas a clientes, como endereços, contatos, agentes, informações de crédito, bancos, documentos, entre outros. O objetivo principal é fornecer uma interface centralizada para visualizar, editar e gerenciar dados de clientes.

* **Tecnologias Utilizadas:**  
  - Delphi (Object Pascal) para desenvolvimento do formulário e lógica de negócios.
  - Componentes visuais como `TsSplitter`, `TsPageControl`, `TsTabSheet`, `TsPanel`, entre outros, para a construção da interface gráfica.
  - Herança de classes para reutilização de funcionalidades (`TFORMkneBaseEdit`).

* **Tipo de Formulário:**  
  Este é um formulário com abas (tabs) que organiza diferentes seções de informações relacionadas ao cliente.  
  - **Elementos do Formulário e Tipos:**  
    - Abas (`TsTabSheet`) para diferentes categorias de informações (ex.: "Mill", "BIA", "Endereços", "Agente", etc.).
    - Botões (`TsBitBtn`) para ações como duplicar registros, imprimir, criar novos registros, etc.
    - Painéis (`TsPanel`) para organizar os componentes visuais.
  - **Ações do Formulário e Efeitos:**  
    - Botão "Duplicar" (`ACTduplicate`): Duplica o registro atual.
    - Botão "Imprimir" (`BTprintCurrentRecordClick`): Imprime o registro atual.
    - Botão "Novo" (`BTNewClick`): Cria um novo registro.

---

## 2. Descrição da Funcionalidade:

* **Ações Específicas:**  
  - Visualizar e editar informações de clientes em diferentes categorias (endereços, contatos, agentes, etc.).
  - Criar novos registros de clientes.
  - Duplicar registros existentes.
  - Imprimir informações do cliente atual.

* **Componentes Principais:**  
  - `TsPageControl` e `TsTabSheet`: Organizam as informações em abas.
  - `TsBitBtn`: Botões para executar ações específicas.
  - Frames como `FRAMEcustProdMill1`, `FRAMElistAddresses1`, etc., que encapsulam funcionalidades específicas.

* **Tradução para Pseudo-código:**  
  - Evento `OnClick` do botão "Imprimir":  
    ```pseudo
    se botão "Imprimir" for clicado então executar função imprimir registro atual.
    ```
  - Evento `OnClick` do botão "Novo":  
    ```pseudo
    se botão "Novo" for clicado então criar novo registro.
    ```
  - Evento `OnExecute` da ação "Duplicar":  
    ```pseudo
    se ação "Duplicar" for executada então duplicar registro atual.
    ```

---

## 3. Lógica Operacional:

* **Fluxo de Execução:**  
  1. Inicialização do formulário (`FormShow`): Carrega os componentes da interface e configurações iniciais.
  2. Interações do usuário, como clicar em botões ou alternar entre abas, disparam eventos que executam funções específicas.
  3. Funções principais:
     - `BTprintCurrentRecordClick`: Localizada no arquivo `Mcustomer.pas`, imprime o registro atual.
     - `ACTduplicateExecute`: Localizada no arquivo `Mcustomer.pas`, duplica o registro atual.
     - `BTNewClick`: Localizada no arquivo `Mcustomer.pas`, cria um novo registro.

* **Dados Necessários:**  
  - Informações do cliente, como nome, endereço, contatos, etc., devem ser preenchidas para criar ou editar registros.

---

## 4. Regras de Negócio:

* **Ações e Pré-condições:**  
  - Botão "Duplicar": Habilitado apenas se houver um registro selecionado.
  - Botão "Imprimir": Habilitado apenas se houver um registro carregado.

* **Filtros Disponíveis:**  
  - Não há filtros explícitos definidos no código fornecido.

* **Mensagens de Erro:**  
  - "Registro não selecionado" se tentar duplicar sem selecionar um registro.
  - "Erro ao carregar dados" se houver falha ao carregar informações do cliente.

* **Valores Padrão dos Campos:**  
  - Não definidos explicitamente no código fornecido.

* **Validações e Condições dos Campos:**  
  - Não há validações explícitas definidas no código fornecido.

---

## 5. Funções Principais:

* **Descrição das Funções:**  
  - `m_getData`: Carrega os dados do cliente no formulário.
  - `m_Validate`: Valida os dados antes de salvar.
  - `m_PutData`: Salva os dados do cliente.
  - `ACTduplicateExecute`: Duplica o registro atual.
  - `BTprintCurrentRecordClick`: Imprime o registro atual.

---

## 6. Consumo de Serviços API:

* Não há chamadas a serviços externos definidas no código fornecido.

---

## 7. Campos Condicionais (Lógica do Formulário):

* Não há campos condicionais explícitos definidos no código fornecido.

---

## 8. Dependências:

* **Bibliotecas Externas:**  
  - `kneCBedit`, `kneFREditSOA`, `kneFRGridEditSOA`, entre outras, para funcionalidades específicas.
  - `cxStyles`, `cxControls`, `cxGrid` para componentes visuais avançados.

* **Componentes Customizados:**  
  - Frames como `FRAMEcustProdMill1`, `FRAMElistAddresses1`, etc., encapsulam funcionalidades específicas.

---

## 9. Listagem de Campos e Validações:

* **Campos no Formulário:**  
  - Nome do Cliente (não definido explicitamente no código).
  - Endereço (não definido explicitamente no código).
  - Contatos (não definido explicitamente no código).

* **Mapeamento de Valores e Colunas do Banco de Dados:**  
  - Não definido explicitamente no código fornecido.

---

## 10. Exemplos e Diagramas:

* **Fluxograma:**  
  Não aplicável devido à ausência de detalhes suficientes.

* **Diagrama de Sequência:**  
  Não aplicável devido à ausência de detalhes suficientes.

* **Exemplo de Código:**  
  ```pascal
  procedure TFORMMcustomer.BTNewClick(Sender: TObject);
  begin
    // Lógica para criar um novo registro
  end;
  ```

* **HTML Representando o Formulário:**  
  ```html
  <div style="width: 1596px; height: 828px; font-family: Verdana;">
    <h1>Customer Maintenance</h1>
    <div style="border: 1px solid #000; padding: 10px;">
      <button>Imprimir</button>
      <button>Novo</button>
      <button>Duplicar</button>
    </div>
    <div style="margin-top: 20px;">
      <ul>
        <li>Mill</li>
        <li>BIA</li>
        <li>Endereços</li>
        <li>Agente</li>
        <li>Informações de Crédito</li>
      </ul>
    </div>
  </div>
  ```

---

## 11. Comentários Importantes no Código:

* `FAddressMasterKeyFields`: Utilizado para definir a ligação com os endereços, pois são compartilhados por várias entidades.
* `FIsPortalActivated`: Comentário indica que este código pode ser removido após a integração do duplicado no `kneCBEdit`.

---

## 12. Conclusão:

O código implementa um formulário robusto para manutenção de clientes, com várias abas para gerenciar diferentes aspectos das informações do cliente. No entanto, faltam detalhes sobre validações de campos e integração com APIs externas. A interface é bem estruturada, mas poderia ser melhor documentada.

---

## 13. Resumo Curto:

O formulário `TFORMMcustomer` gerencia informações de clientes, organizadas em abas para diferentes categorias. Ele permite criar, editar, duplicar e imprimir registros, com uma interface bem estruturada e componentes reutilizáveis.#### **Mcustomer.pas**

```
unit Mcustomer;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, kneCBedit, StdCtrls, Buttons, ExtCtrls, ComCtrls, kneFREditSOA,
  FRcustomer, FRcustProdMill, kneFRGridEditSOA, FRcustBia, DBClient,
  kneFRCtrlEditSOA, knePrivileges, ImgList, sSpeedButton,                                                                      
  sBitBtn, ToolWin, acCoolBar, sPanel, kneEnterAsTab, sPageControl,
  ActnList, sSplitter, FRlistContacts, FRlistAddresses, FRcustAgent,
  FRcustomerCreditInfo, FRcustBank, cxStyles, cxControls, cxGrid, FRchanges,
  FRdocumentsInformation, kneTypes, FRcustSalesMan, FRcustomerVAT,
  FRextShipDelCust, FRentityComm, FRcustIBAN, FRcustOrdUnits,
  FRcustBusinessChannel, FRcustPulpPriceType, FRcustCoC,
  FRcustCrosssellBrand;
  
type
  TFORMMcustomer = class(TFORMkneBaseEdit)
    sSplitter1: TsSplitter;
    PGCcstDetails: TsPageControl;
    TSHmill: TsTabSheet;
    FRAMEcustProdMill1: TFRAMEcustProdMill;
    TSHbia: TsTabSheet;
    FRAMEcustBia1: TFRAMEcustBia;
    SHaddresses: TsTabSheet;
    FRAMElistAddresses1: TFRAMElistAddresses;
    SPL2: TsSplitter;
    FRAMElistContacts1: TFRAMElistContacts;                                                                                                                      
    TSHagent: TsTabSheet;
    FRAMEcustAgent1: TFRAMEcustAgent;
    TSHcredInfo: TsTabSheet;
    FRAMEcustomerCreditInfo1: TFRAMEcustomerCreditInfo;
    TSHbanks: TsTabSheet;
    FRAMEcustBank1: TFRAMEcustBank;
    TSHchanges: TsTabSheet;
    FRAMEchanges1: TFRAMEchanges;
    TSHdocuments: TsTabSheet;
    FRAMEdocumentsInformation1: TFRAMEdocumentsInformation;
    PNL1: TsPanel;
    BTNaddress: TsBitBtn;
    ACTduplicate: TAction;
    TSHcustSalesMan: TsTabSheet;
    FRAMEcustSalesMan1: TFRAMEcustSalesMan;
    TSHcustVAT: TsTabSheet;
    FRAMEcustomerVAT1: TFRAMEcustomerVAT;
    TSHextShipDelCust: TsTabSheet;
    FRAMEextShipDelCust1: TFRAMEextShipDelCust;
    TSHentityComm: TsTabSheet;
    FRAMEentityComm1: TFRAMEentityComm;
    TSHcustIBAN: TsTabSheet;
    FRAMEcustIBAN1: TFRAMEcustIBAN;
    FRAMEcustomer1: TFRAMEcustomer;
    TSHcustOrdUnits: TsTabSheet;
    FRAMEcustOrdUnits1: TFRAMEcustOrdUnits;
    TSHbusinessChannel: TsTabSheet;
    FRAMEcustBusinessChannel1: TFRAMEcustBusinessChannel;
    TSHpulpPriceType: TsTabSheet;
    TSHcustCoC: TsTabSheet;
    FRAMEcustCoC1: TFRAMEcustCoC;
    TSHcrosssellingBrands: TsTabSheet;
    FRAMEcustCrosssellBrand1: TFRAMEcustCrosssellBrand;
    FRAMEcustPulpPriceType1: TFRAMEcustPulpPriceType;
    procedure BTprintCurrentRecordClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure BTCancelClick(Sender: TObject);
    procedure ACTduplicateExecute(Sender: TObject);
    procedure BTNewClick(Sender: TObject);
    procedure PNbotoesClick(Sender: TObject);
    procedure PGCcstDetailsChange(Sender: TObject);
  private
    { Private declarations }
    FAddressMasterKeyFields: String;   // � necess�ria para definir a liga��o com os address ( uma vez que estes s�o utilizados por varias entidades)
    FcanDuplicate: Boolean;
    FIsPortalActivated: Boolean;  //JAR #5354 23-02-2010 Apagar este c�digo apos int. o duplicate na kneCBEdit

    function GetAddrMasterKeyFields: String;
    procedure m_SetContactsDataSet(sender: TObject);
    procedure m_ReloadedRecord(Sender: TObject);
    procedure SetIsPortalActivated(const Value: Boolean);
    function GetIsPortalActivated: Boolean;

  protected
    procedure m_getData; override;
    function m_Validate: Boolean; override;
    procedure m_PutData; override;

  public
    { Public declarations }
    constructor Create(AOwner: TComponent);

    class function m_CreateFormEdit(const AOwner: TComponent): TFORMkneBaseEdit; override;
    procedure m_FormEdit(const pv_AccessMode: string; const pv_KeyValues: string = '');  override;

    procedure BIAControl;
    procedure DoProtectControl(Sender: TObject;
      const pv_Args: array of const);
    function SetAllControlsState(const pv_Control: TControl;
      const pv_Enabled: Boolean): Boolean;
    procedure SetProtectFieldsState(Sender: TFRAMEBaseGridEditSOA);
```

#### **Mcustomer.dfm**

```
inherited FORMMcustomer: TFORMMcustomer
  Left = 0
  Top = 103
  Width = 1596
  Height = 828
  Caption = 'Customer Maintenance'
  Constraints.MinHeight = 690
  Constraints.MinWidth = 1050
  Font.Charset = ANSI_CHARSET
  Font.Name = 'Verdana'
  OldCreateOrder = True
  PixelsPerInch = 96
  TextHeight = 13
  object sSplitter1: TsSplitter [1]
    Left = 0
    Top = 507
    Width = 1580
    Height = 4
    Cursor = crVSplit
    Align = alTop
    SkinData.SkinSection = 'FORM'
  end
  inherited PNLtoolbar: TsPanel
    Width = 1580
    TabOrder = 2
    inherited CLBactions: TsCoolBar
      Width = 1580
      Bands = <
        item
          Control = PNbotoes
          ImageIndex = -1
          MinHeight = 41
          Width = 1576
        end>
      inherited PNbotoes: TsPanel
        Width = 1563
        OnClick = PNbotoesClick
        inherited BTNseparator2: TsSpeedButton
          Left = 642
        end
        inherited PNLdelete: TsPanel
          inherited BTNDelete: TsBitBtn
            Glyph.Data = {
              36090000424D3609000000000000360000002800000018000000180000000100
              2000000000000009000000000000000000000000000000000000FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF007B3921006331
              310063313100633131006331310063313100FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF006331310031319C0031319C003131
              CE0031319C003131CE0031319C0031319C003131630031313100FF00FF00FF00
              FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00FF00FF009C5A390031319C003131CE003131CE003131CE00315A
              E700315AE700315AE7003131CE003131CE003131CE0031319C0031319C003131
              3100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF00FF00FF00633163003131CE003131CE00315AE700315AE700315AE700315A
              E700315AE700315AE700315AE700315AE700315AE700315AE7003131CE003131
              9C0031313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
              FF009C6363003131CE00315AE700315AE700315AE700315AE700315AE700315A
              E700315AE700315AE700315AE700315AE700315AE700315AE700315AE7003163
              CE003131CE0031313100FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00CE63
              63003131CE00315AE700315AE700315AE700315AE7003163FF00315AE700315A
              E700315AE700315AE700315AE700315AE700315AE700315AE700315AE700315A
              E7003131CE003131CE0031313100FF00FF00FF00FF00FF00FF00FF00FF003131
              9C00315AE700315AE700315AE700315AE700315AE700315AE700315AE700315A
              E700315AE700315AE700315AE700315AE7003163FF00315AE700315AE7003163
              FF00315AE7003131CE0031319C0063313100FF00FF00FF00FF00B5735A00315A
              E700315AE700315AE700315AE700315AE700315AE700315AE700315AE700315A
              E700315AE7003163FF00315AE700315AE700315AE700315AE700315AE700315A
              E700315AE700315AE7003131CE0031313100FF00FF00FF00FF0063319C00315A
              E700315AE700315AE700315AE700315AE700315AE700315AE700315AE7003163
              FF00315AE700315AE700315AE700315AE700315AE700315AE700315AE700315A
              E700315AE700315AE7003131CE0031319C007B392100FF00FF003163CE00315A
              E700315AE700315AE700315AE700315AE7003163FF00315AE700315AE700315A
              E700315AE700315AE700315AE700315AE700315AE700315AE700315AE700315A
              E700315AE700315AE700315AE7003131CE0063310000FF00FF00315AE700315A
              E7003163FF003163FF00CEEFF700CECEFF00CECEFF00CECEFF00CECEFF00CECE
              FF00CECEFF00CECEFF00CECEFF00CECEFF00CECEFF00CECEFF00CECEFF00CECE
              FF00315AE700315AE700315AE7003131CE0063313100FF00FF00315AE700315A
              E700315AE700315AE700FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00315AE700315AE700315AE7003163CE0063313100FF00FF00315AE700315A
              E700319CFF003163FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00315AE700315AE700315AE7003131CE0063313100FF00FF00315AE700315A
              E7006363FF006363CE00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFFFF00FFFF
              FF00315AE700315AE700315AE700315AE7007B392100FF00FF00315AE700315A
              E700639CFF00639CFF00639CFF00639CFF00639CCE00639CFF006363FF00639C
              FF006363FF003163FF003163CE003163FF003163CE00315AE700315AE700315A
              E700315AE700315AE700315AE7003131CE0094422900FF00FF0063639C00315A
              E700639CFF00639CFF00639CCE00639CFF00639CFF00639CFF00639CCE00639C
              FF00639CCE00639CFF006363FF003163FF003163FF003163FF00315AE7003163
              FF00315AE700315AE700315AE70031319C009C5A3900FF00FF00CE636300315A
              E7006363FF00639CFF00639CFF00639CFF009C9CFF00639CFF00639CFF00639C
              FF00639CFF006363FF00639CCE006363FF00319CCE003163FF00315AE700315A
              E700315AE700315AE700315AE70063313100FF00FF00FF00FF00FF00FF006363
              CE00315AE700639CFF009C9CFF00A5B5F700639CFF009C9CFF00639CFF009C9C
```
<!-- tabs:end -->

